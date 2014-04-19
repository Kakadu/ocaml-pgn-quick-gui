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
end

type id_board_map = (int, string * Types.Board.t * int) Hashtbl.t

type options = {
  mutable controller: controller_t;
  mutable board_model: abstractListModel;
  cells: DataItem.base_DataItem array;
  mutable games : Types.game list;
  mutable curGame: Types.game;
  mutable cur_position: int;
  mutable boardHash: id_board_map;
}


let options =
  { controller = Obj.magic 1
  ; cells = Array.create 64 (Obj.magic 2)
  ; board_model = Obj.magic 1
  ; games = []
  ; curGame = Obj.magic 1
  ; cur_position = 0
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
(*
let init_board board =
  (* 1st cell is a8 *)
  let set1 (color,sort) n : unit = board.(n) <- `Figure (color,sort) in
  for i=8 to 8+7 do set1 (Black, `Pawn) i done;
  for i=48 to 48+7 do set1 (White, `Pawn) i done;
  List.iter (set1 (Black, `Rook)) [0; 7];
  List.iter (set1 (Black, `Knight)) [1; 6];
  List.iter (set1 (Black, `Bishop)) [2; 5];
  set1 (Black, `Queen) 3;
  set1 (Black, `King) 4;
  List.iter (set1 (White, `Knight)) [57; 62];
  List.iter (set1 (White, `Rook)) [63; 56];
  List.iter (set1 (White, `Bishop)) [58; 61];
  set1 (White, `Queen) 59;
  set1 (White, `King) 60
  *)

let game_for_qt root : string * id_board_map =
  let html_buf = Buffer.create 100 in
  let map = Hashtbl.create 100 in

  let initial_board = Board.create () in
  let (>>=) = Option.(>>=) in
  let rec helper (board: (int * color * Board.t) option) root =
    let () = match board with
      | Some (n,White,_) -> bprintf html_buf "%d. " n;
      | _  -> ()
    in
    Buffer.add_string html_buf (string_of_move root.move);
    Buffer.add_string html_buf " ";
    let init = board >>= fun ((n,_,_) as b) -> Board.make_move root.move b in
    let f : (int * color * Board.t) option -> _ -> (int * color * Board.t) option =
      fun acc -> function
      | `Continue x -> helper acc x   (* TODO *)
      | `NullMoves _ -> acc
    in
    (match root.next with
    | `NullMoves _ -> init
    | `Result _ -> init
    | `Continue root2 -> helper init root2) >>= fun _ ->
    List.fold_left ~f ~init:board root.variants
  in
  let _ = helper (Some (1, White, Board.create ())) root in
  (Buffer.contents html_buf, map)

let main () =
  let () = for n=0 to 63 do
    let cpp_item = DataItem.create_DataItem () in
    let o = object(self)
        inherit DataItem.base_DataItem cpp_item
        val mutable pic_ = ""
        method setPicture s =
          if s <> pic_ then ( pic_ <- s;
                              printf "Setting picture = `%s` on index `%d`\n%!" s n;
                              self#emit_pictureChanged s )
        method getPicture () =
          (* N.B. While changing array we should send events about changing *)
          let (_,curBoard,_) = Hashtbl.find options.boardHash options.cur_position in
          match Types.Board.get_celli_value curBoard (n mod 8, n/8) with
          | None -> ""
          | Some (color,fig) -> getPicture (color, fig)

        method isWhite () =
          if n mod 16 >= 8 then (n mod 2 <> 0)
          else (n mod 2 = 0)
    end in
    options.cells.(n) <- o;
  done
  in

  Hashtbl.add options.boardHash 0 ("init",Types.Board.create (), 0);

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

    method doE2E4 () =
      print_endline "e2e4"; (*
      let from = index_of_field 'e' 2
      and dest = index_of_field 'e' 4 in
      (options.cells.(from))#setPicture "";
      (options.cells.(dest))#setPicture (getPicture White `Pawn); *)
      ()

    method restartGame () = ()
    method nextMove () = ()
    method prevMove () = ()
  end
  in
  options.controller <- controller;
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler;
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"boardModel" model#handler;

  let data = Pgn.parse_file "game.pgn" |> Types.Option.get_exn |> List.hd_exn |> snd in
  print_endline "File parsed!";
  let (text,_) = game_for_qt data in
  print_endline text;
  print_endline "Setting text";
  options.controller#setMovesText text;
  ()


let () =
  run_with_QQmlApplicationEngine Sys.argv main "ui/Root.qml"
