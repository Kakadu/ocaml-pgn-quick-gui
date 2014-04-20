open Printf
open Helpers
open QmlContext
open Pgn
open Types

let () = Printexc.record_backtrace true

class virtual abstractListModel cppobj = object(self)
  inherit AbstractModel.base_AbstractModel cppobj as super
  method parent _ = QModelIndex.empty
  method index row column parent =
    if (row>=0 && row<self#rowCount parent) then QModelIndex.make ~row ~column:0
    else QModelIndex.empty
  method columnCount _ = 1
  method hasChildren _ = self#rowCount QModelIndex.empty > 0
end

class type virtual controller_t = object
  inherit Controller.base_Controller
  method setMovesText: string -> unit
  method nextMove : unit -> unit
  method prevMove : unit -> unit
  method restartGame : unit -> unit
  method emit_positionChanged: unit
end

type board_hash_key = string
type id_board_map = (board_hash_key, string * Types.Board.t * board_hash_key) Hashtbl.t

class type virtual dataItem_t = object
  inherit DataItem.base_DataItem
  method update_picture : unit
end
type options = {
  mutable controller: controller_t;
  mutable board_model: abstractListModel;
  cells: dataItem_t array;
  mutable games : Types.game list;
  mutable curGame: Types.game;
  mutable cur_position: board_hash_key;
  mutable boardHash: id_board_map;
}


let options =
  { controller = Obj.magic 1
  ; cells = Array.create 64 (Obj.magic 2)
  ; board_model = Obj.magic 1
  ; games = []
  ; curGame = Obj.magic 1
  ; cur_position = ""
  ; boardHash = Hashtbl.create 64
  }

let getPicture (color,fig)  =
  sprintf "Chess_%c%ct45.svg" (Char.lowercase @@ Types.char_of_figure fig)
    (match color with Types.White -> 'l' | Black -> 'd')

let index_of_field v h =
  assert (h>=1 && h<=8);
  assert (List.mem v ['a';'b';'c';'d';'e';'f';'g';'h']);
  let ans = (int_of_char v - int_of_char 'a') + 8 * (8-h) in
  assert (ans>=0 && ans < 64);
  ans

type path_t = [ `Forward of int | `Turn of int ]
let string_of_path (xs: path_t list) =
  let b = Buffer.create 100 in
  List.iter (function
    | `Forward n -> bprintf b "f%d" n
    | `Turn n -> bprintf b "t%d" n
  ) xs;
  Buffer.contents b

let append_path p ps =
  match (p,ps) with
  | `Turn x,_ -> `Turn x :: ps
  | `Forward, (`Forward x)::xs -> `Forward (x+1) :: xs
  | _ -> `Forward 1 :: ps

let game_for_qt root : string * id_board_map =
  let html_buf = Buffer.create 100 in
  let map = Hashtbl.create 100 in

  let initial_board = Board.create () in
  let (>>=) = Option.(>>=) in
  let (>|=) = Option.(>|=) in
  let rec helper (board: (int * color * Board.t) option) path root =
    let key = string_of_path path in
    let () = match board with
      | Some (n,White,_board) ->
        bprintf html_buf "%d. " n;
      | _  -> ()
    in
    let next_path = append_path `Forward path in
    bprintf html_buf "<a href='%s'>%s</a> " (string_of_path next_path) (string_of_move root.move);

    let init = board >>= Board.make_move root.move >|= fun ((n,_,_board) as b) ->
      printf "Add to hash %s -> (_,_,%s)\n%!" (string_of_path next_path) (string_of_path path);
      Hashtbl.add map (string_of_path next_path) ("",_board, string_of_path path);
      b
    in

    let f : (int * color * Board.t) option -> varn:int -> _ -> unit =
      fun acc ~varn -> function
      | `Continue x -> helper acc (`Turn varn :: path) x
      | `NullMoves _ -> ()
    in
    List.iteri root.variants ~f:(fun varn move ->
      bprintf html_buf "(";
      let () = match board with
        | Some (n,Black,_) -> bprintf html_buf "%d..." n
        | _ -> ()
      in
      f board ~varn move;
      bprintf html_buf ")"
    );

    match root.next with
    | `NullMoves _ -> ()
    | `Result _ -> ()
    | `Continue root2 -> helper init next_path root2
  in
  let _ = helper (Some (1, White, Board.create ())) [`Forward 0] root in
  (Buffer.contents html_buf, map)

let main () =
  let () = for n=0 to 63 do
    let cpp_item = DataItem.create_DataItem () in
    let o = object(self)
        inherit DataItem.base_DataItem cpp_item
        val mutable pic_ = ""
        method setPicture s =
          if s <> pic_ then ( pic_ <- s;
                              (*printf "Setting picture = `%s` on index `%d`\n%!" s n;*)
                              self#emit_pictureChanged s )
        method getPicture () =
          (* N.B. While changing array we should send events about changing *)
          let (_,curBoard,_) = Hashtbl.find options.boardHash options.cur_position in
          match Types.Board.get_celli_value curBoard ((n mod 8), 7-(n/8)) with
          | None -> ""
          | Some (color,fig) -> getPicture (color, fig)

        method isWhite () =
          if n mod 16 >= 8 then (n mod 2 <> 0)
          else (n mod 2 = 0)
        method update_picture = self#setPicture (self#getPicture ())
    end in
    options.cells.(n) <- o;
  done
  in

  Hashtbl.add options.boardHash "" ("init",Types.Board.create (), "");

  let cpp_model = AbstractModel.create_AbstractModel () in
  let myDefaultRoleMainModel = 555 in
  AbstractModel.add_role cpp_model myDefaultRoleMainModel "homm";

  let model = object(self)
    inherit abstractListModel cpp_model as super
    method rowCount _ = 64
    method data index role =

      let n = QModelIndex.row index in
      if (n<0 || n>= 64) then QVariant.empty
      else begin
        if (role=0 || role = myDefaultRoleMainModel) (* DisplayRole *)
        then QVariant.of_object (options.cells.(n))#handler
        else QVariant.empty
      end

    method emit_all =
      self#report_dataChanged (QModelIndex.make ~row:0 ~column:0) (QModelIndex.make ~row:7 ~column:7);
      for i=0 to 63 do options.cells.(i)#update_picture done
  end
  in

  let controller_cppobj = Controller.create_Controller () in
  let controller = object(self)
    inherit Controller.base_Controller controller_cppobj as super
    val mutable moves_text = ""
    method isHasData () = moves_text<>""
    method getMovesText () = moves_text

    method setMovesText v =
      if v<> moves_text then (moves_text <- v; self#emit_movesTextChanged v)

    method moveSelected key =
      printf "moveSelected %s\n" key;
      (*print_endline @@ Hashtbl.string_of_keys (fun x -> x) options.boardHash;*)
      options.cur_position <- key;
      model#emit_all;
      ()

    method doE2E4 () =
      print_endline "e2e4"; (*
      let from = index_of_field 'e' 2
      and dest = index_of_field 'e' 4 in
      (options.cells.(from))#setPicture "";
      (options.cells.(dest))#setPicture (getPicture White `Pawn); *)
      ()

    method emit_positionChanged = model#emit_all
    method restartGame () = ()
    method nextMove () = ()
    method prevMove () = ()
  end
  in
  options.controller <- controller;
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler;
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"boardModel" model#handler;

  let filename = List.nth ~n:1 ["game.pgn"; "ChigorinSteinitz.pgn"] in
  let data = Pgn.parse_file filename |> Types.Option.get_exn |> List.hd_exn |> snd in
  print_endline "File parsed!";
  let (text,map) = game_for_qt data in
  Hashtbl.merge options.boardHash map;
  print_endline text;
  options.controller#setMovesText text;
  ()


let () =
  run_with_QQmlApplicationEngine Sys.argv main "ui/Root.qml"
