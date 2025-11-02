open! Js_of_ocaml
open Patchwork_logic_library
open Patchwork
module C = Js_of_ocaml.Console

let log s = C.(console##log (Js.string s))
let no_priority : Js.js_string Js.t Js.optdef = Js.undefined

let get_svg_image_exn id : Dom_svg.imageElement Js.t =
  let el = Dom_html.getElementById_exn id in
  (Js.Unsafe.coerce el : Dom_svg.imageElement Js.t)

let unwrap_exn opt =
  match opt with
  | Some x -> x
  | None -> failwith "Tried to unwrap None"

let patch_img_overlay_offests (p : Patch.patch_shape) =
  match p with
  | Square -> (25.0, 25.0)
  | SquareNub -> (25.0, 82.0)
  | SquareHighFive -> (16.0, 50.0)
  | TCross -> (16.5, 50.0)
  | S -> (16.5, 90.0)
  | LongI -> (10.0, 50.0)
  | LHalfH -> (12.5, 25.0)
  | SHalfH -> (16.0, 25.0)
  | H -> (16.0, 16.0)
  | Corner -> (25.0, 25.0)
  | CornerRev -> (25.0, 25.0)
  | SLVert -> (25.0, 12.5)
  | ShortI -> (25.0, 20.0)
  | LRev -> (25.0, 82.0)
  | LongL -> (25.0, 12.5)
  | L -> (25.0, 16.0)
  | ChunkyLRev -> (25.0, 50.0)
  | SmallI -> (50.0, 25.0)
  | I -> (50.0, 12.5)
  | ShortT -> (16.0, 25.0)
  | StubbyT -> (16.0, 25.0)
  | T -> (16.0, 12.5)
  | Plus -> (16.0, 50.0)
  | Zig -> (25.0, 50.0)
  | ZigZag -> (16.0, 16.0)
  | ZigRev -> (25.0, 16.0)
  | ChunkyZig -> (25.0, 37.5)
  | Cross -> (25.0, 50.0)
  | INub -> (25.0, 70.0)
  | WideStubbyT -> (12.5, 25.0)
  | Prong -> (16.0, 72.5)
  | Vine -> (16.0, 37.5)
  | WidePlus -> (12.5, 50.0)
  | Empty -> (0.0, 0.0)

let _main_board_positions player slot =
  if player = 1 then
    match slot with
    | 1 -> (24.0, 18.0, 8.0)
    | 2 -> (32.0, 15.0, 8.0)
    | 3 -> (40.7, 14.0, 8.0)
    | 4 -> (49.0, 14.0, 8.0)
    | 5 -> (57.8, 15.0, 8.0)
    | 6 -> (66.0, 19.0, 8.0)
    | 7 -> (71.0, 26.0, 8.0)
    | 8 -> (74.0, 34.0, 8.0)
    | 9 -> (75.0, 43.0, 8.0)
    | 10 -> (74.5, 50.0, 8.0)
    | 11 -> (73.5, 58.0, 8.0)
    | 12 -> (71.3, 65.0, 8.0)
    | 13 -> (66.0, 71.0, 8.0)
    | 14 -> (59.5, 74.0, 8.0)
    | 15 -> (52.3, 77.4, 8.0)
    | 16 -> (46.2, 78.0, 7.0)
    | 17 -> (38.7, 77.0, 8.0)
    | 18 -> (33.0, 75.0, 7.5)
    | 19 -> (25.5, 73.0, 8.0)
    | 20 -> (21.0, 68.0, 8.0)
    | 21 -> (19.0, 61.5, 8.0)
    | 22 -> (17.0, 56.0, 8.0)
    | 23 -> (19.0, 55.0, 8.0)
    | 24 -> (20.0, 44.0, 8.0)
    | 25 -> (24.0, 37.0, 8.0)
    | 26 -> (29.5, 33.0, 8.0)
    | 27 -> (35.2, 29.5, 8.0)
    | 28 -> (41.5, 29.0, 7.0)
    | 29 -> (48.0, 29.0, 7.0)
    | 30 -> (54.5, 31.0, 7.0)
    | 31 -> (60.0, 36.0, 6.0)
    | 32 -> (64.0, 41.0, 6.0)
    | 33 -> (65.0, 45.0, 6.0)
    | 34 -> (63.0, 55.0, 6.0)
    | 35 -> (61.0, 59.5, 6.0)
    | 36 -> (52.0, 65.0, 6.0)
    | 37 -> (45.4, 65.3, 6.0)
    | 38 -> (39.5, 65.3, 6.0)
    | 39 -> (35.0, 60.0, 6.0)
    | 40 -> (33.5, 56.0, 6.0)
    | _ -> (0.0, 0.0, 0.0)
  else
    match slot with
    | 1 -> (20.0, 10.0, 10.0)
    | 2 -> (30.0, 6.0, 10.0)
    | 3 -> (40.0, 5.0, 10.0)
    | 4 -> (50.5, 5.0, 10.0)
    | 5 -> (60.0, 7.0, 10.0)
    | 6 -> (70.0, 11.0, 10.0)
    | 7 -> (76.0, 20.0, 10.0)
    | 8 -> (80.0, 31.0, 10.0)
    | 9 -> (82.0, 41.0, 10.0)
    | 10 -> (81.0, 50.0, 10.0)
    | 11 -> (79.0, 59.0, 10.0)
    | 12 -> (76.0, 68.0, 10.0)
    | 13 -> (70.0, 75.0, 10.0)
    | 14 -> (61.0, 79.0, 10.0)
    | 15 -> (52.0, 83.0, 10.0)
    | 16 -> (44.3, 83.0, 8.5)
    | 17 -> (36.0, 83.0, 9.5)
    | 18 -> (28.0, 81.0, 9.5)
    | 19 -> (20.0, 78.0, 9.5)
    | 20 -> (14.0, 71.0, 10.0)
    | 21 -> (10.0, 63.5, 10.0)
    | 22 -> (8.0, 55.5, 10.0)
    | 23 -> (9.0, 48.0, 10.0)
    | 24 -> (13.0, 39.0, 10.0)
    | 25 -> (18.0, 31.0, 10.0)
    | 26 -> (25.0, 25.0, 10.0)
    | 27 -> (25.5, 73.0, 10.0)
    | 28 -> (40.5, 21.0, 9.0)
    | 29 -> (48.5, 22.0, 9.0)
    | 30 -> (56.2, 24.5, 9.0)
    | 31 -> (62.0, 31.0, 9.0)
    | 32 -> (66.0, 39.0, 7.0)
    | 33 -> (66.0, 49.0, 6.5)
    | 34 -> (66.0, 57.0, 6.5)
    | 35 -> (60.5, 63.5, 6.5)
    | 36 -> (52.0, 69.5, 6.5)
    | 37 -> (44.5, 69.5, 6.5)
    | 38 -> (37.0, 68.5, 6.5)
    | 39 -> (32.5, 64.0, 6.5)
    | 40 -> (28.0, 57.0, 6.5)
    | _ -> (0.0, 0.0, 0.0)

let preload_images () =
  let svgs =
    [|
      "svgs/Main_Board.svg";
      "svgs/Neutral.svg";
      "svgs/Pyramid.svg";
      "svgs/Cube.svg";
      "svgs/1_stack_buttons.svg";
      "svgs/logo.svg";
      "svgs/P1_quilt_board.svg";
      "svgs/WidePlus.svg";
      "svgs/Prong.svg";
      "svgs/INub.svg";
      "svgs/Cross.svg";
      "svgs/ZigZag.svg";
      "svgs/T.svg";
      "svgs/ShortT.svg";
      "svgs/I.svg";
      "svgs/SmallI.svg";
      "svgs/LRev.svg";
      "svgs/Corner.svg";
      "svgs/LHalfH.svg";
      "svgs/LongI.svg";
      "svgs/TCross.svg";
      "svgs/SquareHighFive.svg";
      "svgs/SquareNub.svg";
      "svgs/letters/a.svg";
      "svgs/letters/b.svg";
      "svgs/letters/c.svg";
      "svgs/letters/d.svg";
      "svgs/letters/e.svg";
      "svgs/letters/f.svg";
      "svgs/letters/g.svg";
      "svgs/letters/h.svg";
      "svgs/letters/i.svg";
      "svgs/letters/j.svg";
      "svgs/letters/k.svg";
      "svgs/letters/l.svg";
      "svgs/letters/m.svg";
      "svgs/letters/n.svg";
      "svgs/letters/o.svg";
      "svgs/letters/p.svg";
      "svgs/letters/q.svg";
      "svgs/letters/r.svg";
      "svgs/letters/s.svg";
      "svgs/letters/t.svg";
      "svgs/letters/u.svg";
      "svgs/letters/v.svg";
      "svgs/letters/w.svg";
      "svgs/letters/x.svg";
      "svgs/letters/y.svg";
      "svgs/letters/z.svg";
    |]
  in
  let preload_div = Dom_html.getElementById_exn "preload_images" in
  Array.iter
    (fun path ->
      let img = Dom_html.createImg Dom_html.document in
      img##.src := Js.string path;
      img##.style##.display := Js.string "none";
      Dom.appendChild preload_div img)
    svgs

let initialize_game pname = _init pname
let current_state : Game_state.t option ref = ref None
let pending_patch_index : int option ref = ref None
let cursor_switch : bool ref = ref false
let patch_1_choice_index : int ref = ref 0
let patch_2_choice_index : int ref = ref 0
let patch_3_choice_index : int ref = ref 0
let cursor_rot_deg : int ref = ref 0

let _delay_ms ms f =
  ignore
    (Dom_html.window##setTimeout (Js.wrap_callback f)
       (Js.number_of_float (float_of_int ms)))

let create_quilt_board_grid () =
  let qb_rep = Dom_html.getElementById_exn "qb_replica" in
  for x = 1 to 9 do
    for y = 1 to 9 do
      let div = Dom_html.createDiv Dom_html.document in
      div##.className := Js.string "replica-cell";
      ignore
        (div##.style##setProperty
           (Js.string "--row")
           (Js.string (string_of_int x))
           no_priority);
      ignore
        (div##.style##setProperty
           (Js.string "--col")
           (Js.string (string_of_int y))
           no_priority);
      Dom.appendChild qb_rep div
    done
  done

let show_nameplate_for_turn () =
  let player_nameplate = Dom_html.getElementById_exn "player_name_turn_greeting" in
  let ai_nameplate = Dom_html.getElementById_exn "ai_name_turn_greeting" in
  if (unwrap_exn !current_state).turn.player_num = 1 then (
    player_nameplate##.style##.display := Js.string "flex";
    ai_nameplate##.style##.display := Js.string "none")
  else (
    player_nameplate##.style##.display := Js.string "none";
    ai_nameplate##.style##.display := Js.string "flex")

let set_player_name name nameplate_id =
  let ai_name_div = Dom_html.getElementById_exn "ai_name_turn_greeting" in
  let name_div = Dom_html.getElementById_exn nameplate_id in
  let frag_ai = Dom_html.document##createDocumentFragment in
  let img = Dom_html.createImg Dom_html.document in
  img##.className := Js.string "glyph";
  img##.src := Js.string "svgs/letters/a.svg";
  ignore (Dom.appendChild (frag_ai :> Dom.node Js.t) (img :> Dom.node Js.t));
  let img2 = Dom_html.createImg Dom_html.document in
  img2##.className := Js.string "glyph";
  img2##.src := Js.string "svgs/letters/i.svg";
  ignore (Dom.appendChild (frag_ai :> Dom.node Js.t) (img2 :> Dom.node Js.t));
  ignore (Dom.appendChild (ai_name_div :> Dom.node Js.t) (frag_ai :> Dom.node Js.t));

  let frag = Dom_html.document##createDocumentFragment in
  for i = 0 to String.length name - 1 do
    let letter_svg =
      match name.[i] with
      | 'a'
      | 'A' ->
          "svgs/letters/a.svg"
      | 'b'
      | 'B' ->
          "svgs/letters/b.svg"
      | 'c'
      | 'C' ->
          "svgs/letters/c.svg"
      | 'd'
      | 'D' ->
          "svgs/letters/d.svg"
      | 'e'
      | 'E' ->
          "svgs/letters/e.svg"
      | 'f'
      | 'F' ->
          "svgs/letters/f.svg"
      | 'g'
      | 'G' ->
          "svgs/letters/g.svg"
      | 'h'
      | 'H' ->
          "svgs/letters/h.svg"
      | 'i'
      | 'I' ->
          "svgs/letters/i.svg"
      | 'j'
      | 'J' ->
          "svgs/letters/j.svg"
      | 'k'
      | 'K' ->
          "svgs/letters/k.svg"
      | 'l'
      | 'L' ->
          "svgs/letters/l.svg"
      | 'm'
      | 'M' ->
          "svgs/letters/m.svg"
      | 'n'
      | 'N' ->
          "svgs/letters/n.svg"
      | 'o'
      | 'O' ->
          "svgs/letters/o.svg"
      | 'p'
      | 'P' ->
          "svgs/letters/p.svg"
      | 'q'
      | 'Q' ->
          "svgs/letters/q.svg"
      | 'r'
      | 'R' ->
          "svgs/letters/r.svg"
      | 's'
      | 'S' ->
          "svgs/letters/s.svg"
      | 't'
      | 'T' ->
          "svgs/letters/t.svg"
      | 'u'
      | 'U' ->
          "svgs/letters/u.svg"
      | 'v'
      | 'V' ->
          "svgs/letters/v.svg"
      | 'w'
      | 'W' ->
          "svgs/letters/w.svg"
      | 'x'
      | 'X' ->
          "svgs/letters/x.svg"
      | 'y'
      | 'Y' ->
          "svgs/letters/y.svg"
      | 'z'
      | 'Z' ->
          "svgs/letters/z.svg"
      | _ -> "svgs/letters/space.png"
    in
    let img = Dom_html.createImg Dom_html.document in
    img##.className := Js.string "glyph";
    img##.src := Js.string letter_svg;
    ignore (Dom.appendChild (frag :> Dom.node Js.t) (img :> Dom.node Js.t))
  done;
  ignore (Dom.appendChild (name_div :> Dom.node Js.t) (frag :> Dom.node Js.t))

let button_image_get_src num_buttons =
  if num_buttons >= 50 then "svgs/50_stack_buttons.svg"
  else if num_buttons >= 40 then "svgs/40_stack_buttons.svg"
  else if num_buttons >= 30 then "svgs/30_stack_buttons.svg"
  else if num_buttons >= 20 then "svgs/20_stack_buttons.svg"
  else if num_buttons >= 10 then "svgs/10_stack_buttons.svg"
  else if num_buttons >= 5 then "svgs/5_stack_buttons.svg"
  else "svgs/1_stack_buttons.svg"

let set_button_count_and_src ~count ~txt_count_id ~image_id =
  let button_text = Dom_html.getElementById_exn txt_count_id in
  button_text##.textContent := Js.some (Js.string (string_of_int count));
  let image = Dom_html.getElementById_exn image_id in
  image##setAttribute (Js.string "src") (Js.string (button_image_get_src count))

let update_buttons_ui (player : Player.t) player_num =
  let count = player.buttons_owned in
  if player_num = 1 then
    set_button_count_and_src ~count ~txt_count_id:"p1_buttons" ~image_id:"p1_buttons_img"
  else
    set_button_count_and_src ~count ~txt_count_id:"p2_buttons" ~image_id:"p2_buttons_img"

let rec find_patch_name pl i =
  match pl with
  | [] -> "Empty"
  | (hd : Patch.t) :: tl ->
      if hd.pos_around_board = i then
        Sexplib.Sexp.to_string_hum (Patch.sexp_of_patch_shape hd.shape)
      else find_patch_name tl i

let clear_qb_before_painting qb_repl =
  let old_overlays = qb_repl##querySelectorAll (Js.string ".grid-cell-patch-overlay") in
  let len = old_overlays##.length in
  for i = len - 1 downto 0 do
    Js.Opt.iter
      (old_overlays##item i)
      (fun (overlay : Dom_html.element Js.t) ->
        Dom.removeChild qb_repl (overlay :> Dom.node Js.t))
  done

let place_neut_token_on_circle cur_pos =
  let patch_id_name = "ptch" ^ string_of_int cur_pos in
  let patch_id = Dom_html.getElementById_exn patch_id_name in
  let neut_id_name = "neut" ^ string_of_int cur_pos in
  let neut_id = Dom_html.getElementById_exn neut_id_name in
  let tok_id = "neut_token" in
  let neutral_token = Dom_html.getElementById_exn tok_id in
  Dom.removeChild neut_id neutral_token;
  Dom.removeChild patch_id neut_id;
  let new_pos = (unwrap_exn !current_state).neut.pos in
  let new_patch_id_name = "ptch" ^ string_of_int new_pos in
  let new_patch_id = Dom_html.getElementById_exn new_patch_id_name in
  let img = Dom_html.createImg Dom_html.document in
  let neut_div = Dom_html.createDiv Dom_html.document in
  neut_div##.classList##add (Js.string "neutralize");
  neut_div##.id := Js.string ("neut" ^ string_of_int new_pos);
  img##.src := Js.string "svgs/Neutral.svg";
  img##.id := Js.string "neut_token";
  Dom.appendChild neut_div img;
  Dom.appendChild new_patch_id neut_div

let disable_taken_patch_img patch_num =
  let patch_id_name : string = "patch" ^ string_of_int (patch_num - 1) in
  log patch_id_name;
  log (string_of_int (unwrap_exn !current_state).neut.pos);
  let patch_id = Dom_html.getElementById_exn patch_id_name in
  patch_id##.style##.display := Js.string "none"

let flash_button_anim button_up_id button_num =
  let wrapper = Dom_html.getElementById_exn "button_up" in
  wrapper##.classList##add (Js.string "visible");
  let button_up = Dom_html.getElementById_exn button_up_id in
  let button_val_id = if button_up_id = "button_up_p1" then "p1_bn" else "p2_bn" in
  let button_img_id =
    if button_up_id = "button_up_p1" then "p1_bn_img" else "p2_bn_img"
  in
  let button_val = Dom_html.getElementById_exn button_val_id in
  let img = Dom_html.getElementById_exn button_img_id in
  let img = Js.coerce img Dom_html.CoerceTo.img (fun _ -> assert false) in
  let src_str = "svgs/" ^ string_of_int (abs button_num) ^ "_stack_buttons.svg" in
  img##.src := Js.string src_str;
  let sign = if button_num >= 0 then "+" else "" in
  if button_num < 0 then img##.classList##add (Js.string "red-tint") else ();
  button_val##.textContent := Js.some (Js.string (sign ^ string_of_int button_num));
  button_up##.classList##remove (Js.string "visible");
  ignore button_up##.offsetWidth;
  button_up##.classList##add (Js.string "visible");
  ignore
    (Dom_html.window##setTimeout
       (Js.wrap_callback (fun () ->
            wrapper##.classList##remove (Js.string "visible");
            img##.classList##remove (Js.string "red-tint")))
       (Js.number_of_float 2000.))

let paint_quilt_board (qb : Game_board.quilt_board) qb_repl ext_qb =
  let x_base = 11.5 in
  let y_base = 19.0 in
  let x_step = 6.85 in
  let y_step = 6.85 in
  let ext_qb_el = Dom_html.getElementById_exn ext_qb in
  clear_qb_before_painting qb_repl;
  let patches_in_place = qb.patches in

  let rec iterate_pip patches =
    match patches with
    | [] -> ()
    | (row, col, (patch : Patch.patch_shape)) :: tl ->
        let x = x_base +. (x_step *. float_of_int col) in
        let y = y_base +. (y_step *. float_of_int (row - 1)) in
        let x_str = Printf.sprintf "%.2f%%" x in
        let y_str = Printf.sprintf "%.2f%%" y in
        let patch_name = Sexplib.Sexp.to_string_hum (Patch.sexp_of_patch_shape patch) in
        let num_cols, num_rows = Patch.get_col_row patch in
        let w = float_of_int num_cols *. x_step in
        let width = Printf.sprintf "%.2f%%" w in
        let h = float_of_int num_rows *. y_step in
        let height = Printf.sprintf "%.2f%%" h in
        let img_src = "svgs/" ^ patch_name ^ ".svg" in
        let img = Dom_html.createImg Dom_html.document in
        let image = Dom_svg.createImage Dom_svg.document in
        image##setAttribute (Js.string "x") (Js.string x_str);
        image##setAttribute (Js.string "y") (Js.string y_str);
        image##setAttribute (Js.string "width") (Js.string width);
        image##setAttribute (Js.string "height") (Js.string height);
        image##setAttribute (Js.string "preserveAspectRatio") (Js.string "xMinYMin meet");
        image##setAttribute (Js.string "href") (Js.string img_src);
        Dom.appendChild ext_qb_el image;

        img##.src := Js.string img_src;
        img##.className := Js.string "grid-cell-patch-overlay";
        ignore
          (img##.style##setProperty
             (Js.string "--col")
             (Js.string (string_of_int col))
             no_priority);
        ignore
          (img##.style##setProperty
             (Js.string "--row")
             (Js.string (string_of_int row))
             no_priority);
        ignore
          (img##.style##setProperty
             (Js.string "--cols")
             (Js.string (string_of_int num_cols))
             no_priority);
        ignore
          (img##.style##setProperty
             (Js.string "--rows")
             (Js.string (string_of_int num_rows))
             no_priority);
        Dom.appendChild qb_repl img;
        iterate_pip tl
  in
  iterate_pip patches_in_place

let rec place_patch_images_around_circle pl =
  let add_patch_image_src n patch_id =
    let patch_name = find_patch_name pl n in
    let patch_shape = Patch.patch_shape_of_sexp (Sexplib.Sexp.of_string patch_name) in
    let col, row = Patch.get_col_row patch_shape in
    let img_src = "svgs/" ^ patch_name ^ ".svg" in
    let patch_image = Dom_html.getElementById_exn patch_id in
    let patch_cont = Dom_html.getElementById_exn ("ptch" ^ string_of_int (n - 1)) in
    patch_image##setAttribute (Js.string "src") (Js.string img_src);
    ignore
      (patch_cont##.style##setProperty
         (Js.string "--cols")
         (Js.string (string_of_int col))
         no_priority);
    ignore
      (patch_cont##.style##setProperty
         (Js.string "--rows")
         (Js.string (string_of_int row))
         no_priority)
  in
  match pl with
  | [] -> ()
  | hd :: tl ->
      let id = "patch" ^ string_of_int (hd.pos_around_board - 1) in
      add_patch_image_src hd.pos_around_board id;
      place_patch_images_around_circle tl

let check_if_winner () : int * int * int =
  let p1_score, p2_score = Move.score_game (unwrap_exn !current_state) in
  if p1_score > p2_score then (1, p1_score, p2_score)
  else if p2_score > p1_score then (2, p1_score, p2_score)
  else if p1_score = -1 then (-1, -1, -1)
  else (0, p1_score, p2_score)

let print_filled_slots (board : Game_board.quilt_board) =
  let filled = board.filled_squares in
  let rec iter f =
    match f with
    | [] -> log "Done"
    | hd :: tl ->
        log (string_of_int (fst hd) ^ ", " ^ string_of_int (snd hd));
        iter tl
  in
  iter filled

let announce_winner w name p1score p2score =
  let results_div = Dom_html.getElementById_exn "results" in
  let score_div = Dom_html.getElementById_exn "score_report" in
  let winner_div = Dom_html.getElementById_exn "winner_msg" in
  if w = 1 then (
    winner_div##.textContent := Js.some (Js.string (name ^ " Wins!"));
    score_div##.textContent :=
      Js.some
        (Js.string
           ("Score: " ^ string_of_int p1score ^ " vs AI score " ^ string_of_int p2score)))
  else if w = 2 then (
    winner_div##.textContent := Js.some (Js.string "AI Wins!");
    score_div##.textContent :=
      Js.some
        (Js.string
           ("Score: "
           ^ string_of_int p1score
           ^ " vs "
           ^ name
           ^ "'s score "
           ^ string_of_int p2score)))
  else (
    winner_div##.textContent := Js.some (Js.string "It's a tie!");
    score_div##.textContent :=
      Js.some
        (Js.string
           ("Score: " ^ string_of_int p1score ^ " vs AI score " ^ string_of_int p2score)));
  results_div##.style##.display := Js.string "flex";
  results_div##.style##.opacity := Js.string "1"

let set_patch_choice_srcs () =
  let p1_img = Dom_html.getElementById_exn "patch1_image" in
  let p2_img = Dom_html.getElementById_exn "patch2_image" in
  let p3_img = Dom_html.getElementById_exn "patch3_image" in

  patch_1_choice_index :=
    Patch.get_one (unwrap_exn !current_state).neut.pos
      (unwrap_exn !current_state).patches_remaining
      (unwrap_exn !current_state).patches_remaining;
  patch_2_choice_index :=
    Patch.get_one !patch_1_choice_index (unwrap_exn !current_state).patches_remaining
      (unwrap_exn !current_state).patches_remaining;
  patch_3_choice_index :=
    Patch.get_one !patch_2_choice_index (unwrap_exn !current_state).patches_remaining
      (unwrap_exn !current_state).patches_remaining;
  let p1_name =
    find_patch_name (unwrap_exn !current_state).patches !patch_1_choice_index
  in
  let p2_name =
    find_patch_name (unwrap_exn !current_state).patches !patch_2_choice_index
  in
  let p3_name =
    find_patch_name (unwrap_exn !current_state).patches !patch_3_choice_index
  in
  let p1_cost = Dom_html.getElementById_exn "patch1_cost" in
  let p1_time = Dom_html.getElementById_exn "patch1_time" in
  let p2_cost = Dom_html.getElementById_exn "patch2_cost" in
  let p2_time = Dom_html.getElementById_exn "patch2_time" in
  let p3_cost = Dom_html.getElementById_exn "patch3_cost" in
  let p3_time = Dom_html.getElementById_exn "patch3_time" in
  let p1_shape = Patch.patch_shape_of_sexp (Sexplib.Sexp.of_string p1_name) in
  let p2_shape = Patch.patch_shape_of_sexp (Sexplib.Sexp.of_string p2_name) in
  let p3_shape = Patch.patch_shape_of_sexp (Sexplib.Sexp.of_string p3_name) in
  let p1c, p1t = Patch.get_values p1_shape in
  let p2c, p2t = Patch.get_values p2_shape in
  let p3c, p3t = Patch.get_values p3_shape in
  p1_cost##.textContent := Js.some (Js.string (string_of_int p1c ^ " buttons"));
  p2_cost##.textContent := Js.some (Js.string (string_of_int p2c ^ " buttons"));
  p3_cost##.textContent := Js.some (Js.string (string_of_int p3c ^ " buttons"));
  p1_time##.textContent := Js.some (Js.string (string_of_int p1t ^ " steps"));
  p2_time##.textContent := Js.some (Js.string (string_of_int p2t ^ " steps"));
  p3_time##.textContent := Js.some (Js.string (string_of_int p3t ^ " steps"));
  let p1_src = "svgs/" ^ p1_name ^ ".svg" in
  let p2_src = "svgs/" ^ p2_name ^ ".svg" in
  let p3_src = "svgs/" ^ p3_name ^ ".svg" in
  p1_img##setAttribute (Js.string "src") (Js.string p1_src);
  p2_img##setAttribute (Js.string "src") (Js.string p2_src);
  p3_img##setAttribute (Js.string "src") (Js.string p3_src)

let position_token_on_board (img : Dom_svg.imageElement Js.t) n =
  let state = unwrap_exn !current_state in
  let x, y, s =
    if n = 1 then _main_board_positions 1 state.tk1.position
    else _main_board_positions 2 state.tk2.position
  in
  let x_str = Printf.sprintf "%.0f%%" x |> Js.string in
  let y_str = Printf.sprintf "%.0f%%" y |> Js.string in
  let sx_str = Printf.sprintf "%.0f%%" s |> Js.string in
  let sy_str = Printf.sprintf "%.0f%%" s |> Js.string in
  img##setAttribute (Js.string "x") x_str;
  img##setAttribute (Js.string "y") y_str;
  img##setAttribute (Js.string "width") sx_str;
  img##setAttribute (Js.string "height") sy_str

let _move_id =
  let cursor_img = Dom_html.getElementById_exn "cursor-patch-img" in
  Dom_html.addEventListener Dom_html.document Dom_html.Event.mousemove
    (Dom_html.handler (fun (ev : #Dom_html.event Js.t) ->
         if not !cursor_switch then Js._true
         else
           Js.Opt.case
             (Dom_html.CoerceTo.mouseEvent (ev :> Dom_html.event Js.t))
             (fun () -> Js._true)
             (fun e ->
               let px (n : Js.number Js.t) =
                 Printf.sprintf "%gpx" (Js.float_of_number n)
               in
               cursor_img##.style##setProperty
                 (Js.string "--x")
                 (Js.string (px e##.clientX))
                 no_priority
               |> ignore;
               cursor_img##.style##setProperty
                 (Js.string "--y")
                 (Js.string (px e##.clientY))
                 no_priority
               |> ignore;
               Js._true)))
    Js._false

let css_var_int style (name : string) : int option =
  let s = Js.to_string (style##getPropertyValue (Js.string name)) |> String.trim in
  match s with
  | "" -> None
  | _ -> (
      try Some (int_of_string s) with
      | _ -> None)

let show_cursor_img ~src ~(cols : int) patch_name =
  let x_off, y_off = patch_img_overlay_offests patch_name in
  let cursor_img = Dom_html.getElementById_exn "cursor-patch-img" in
  cursor_img##setAttribute (Js.string "src") (Js.string src);
  cursor_img##.style##setProperty
    (Js.string "--cols")
    (Js.string (string_of_int cols))
    no_priority
  |> ignore;
  cursor_img##.style##setProperty
    (Js.string "--ox")
    (Js.string (Printf.sprintf "-%.2f%%" x_off))
    no_priority
  |> ignore;
  cursor_img##.style##setProperty
    (Js.string "--oy")
    (Js.string (Printf.sprintf "-%.2f%%" y_off))
    no_priority
  |> ignore;
  cursor_img##.style##setProperty (Js.string "display") (Js.string "block") no_priority
  |> ignore;
  cursor_switch := true

let hide_cursor_img () =
  cursor_switch := false;
  let cursor_img = Dom_html.getElementById_exn "cursor-patch-img" in
  cursor_img##.style##setProperty (Js.string "display") (Js.string "none") no_priority
  |> ignore;
  cursor_img##.style##setProperty (Js.string "--cols") (Js.string "0") no_priority
  |> ignore;
  cursor_img##setAttribute (Js.string "src") (Js.string "")

let rotate_cursor_img () =
  cursor_rot_deg := (!cursor_rot_deg + 90) mod 360;
  let cursor_img = Dom_html.getElementById_exn "cursor-patch-img" in
  cursor_img##.style##setProperty
    (Js.string "--rot")
    (Js.string (string_of_int !cursor_rot_deg ^ "deg"))
    no_priority
  |> ignore

let rec play_ai_turn () =
  log "AI decision maker trigger.";
  match !current_state with
  | None -> ()
  | Some st ->
      if st.turn.player_num = 2 then (
        log "AI TURN";
        let quilt_board_replica_id = Dom_html.getElementById_exn "qb_replica" in
        let current_player = st.turn in
        let ai_move, pos, row, col = Ai_module.determine_move st in
        let old_buttons = current_player.buttons_owned in
        let old_neut_pos = st.neut.pos in
        current_state := Some (Move.choose_move st ai_move pos row col);
        place_neut_token_on_circle old_neut_pos;
        show_nameplate_for_turn ();
        let new_buttons = current_player.buttons_owned - old_buttons in
        paint_quilt_board (unwrap_exn !current_state).p2qb quilt_board_replica_id
          "p2_qb_svg";
        let pyramid_token = get_svg_image_exn "pyramid_tt" in
        position_token_on_board pyramid_token 2;
        update_buttons_ui current_player 2;
        flash_button_anim "button_up_p2" new_buttons;
        if (unwrap_exn !current_state).turn.player_num = 2 then
          _delay_ms 2000 (fun () -> play_ai_turn ()))
      else log "AI SKIPPED"

let () =
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ ->
        preload_images ();
        create_quilt_board_grid ();
        let status_div = Dom_html.getElementById_exn "status-message" in
        status_div##.classList##add (Js.string "hide");
        let loading_div = Dom_html.getElementById_exn "loading-message" in
        loading_div##.classList##add (Js.string "hide");
        let quilt_board_replica_id = Dom_html.getElementById_exn "qb_replica" in
        let intro = Dom_html.getElementById "intro" in
        let container = Dom_html.getElementById "container" in
        let patch_interface = Dom_html.getElementById_exn "choose_patch" in
        let blur = Dom_html.getElementById_exn "blur" in
        let cube_token = get_svg_image_exn "cube_tt" in
        let patch_chosen = Dom_html.getElementById_exn "patch_chosen" in
        let pyramid_token = get_svg_image_exn "pyramid_tt" in
        let advance_button =
          Dom_html.getElementById_coerce "advance" Dom_html.CoerceTo.button
        in
        let place_patch_button =
          Dom_html.getElementById_coerce "place_patch" Dom_html.CoerceTo.button
        in
        let start_game_button =
          Dom_html.getElementById_coerce "start_game" Dom_html.CoerceTo.button
        in
        let go_back_button =
          Dom_html.getElementById_coerce "cancel_patch" Dom_html.CoerceTo.button
        in
        let cancel_place_patch_button =
          Dom_html.getElementById_coerce "cancel_pp" Dom_html.CoerceTo.button
        in

        let choose_patch_1_button =
          Dom_html.getElementById_coerce "choose1" Dom_html.CoerceTo.button
        in
        let choose_patch_2_button =
          Dom_html.getElementById_coerce "choose2" Dom_html.CoerceTo.button
        in
        let choose_patch_3_button =
          Dom_html.getElementById_coerce "choose3" Dom_html.CoerceTo.button
        in
        let player_name =
          Dom_html.getElementById_coerce "player_name" Dom_html.CoerceTo.input
        in
        intro##.style##.display := Js.string "block";
        container##.style##.display := Js.string "none";
        let cursor_img = Dom_html.getElementById_exn "cursor-patch-img" in
        ignore
          (Dom_html.addEventListener cursor_img Dom_html.Event.mousedown
             (Dom_html.handler (fun ev ->
                  let ev = (Js.Unsafe.coerce ev : Dom_html.mouseEvent Js.t) in
                  if ev##.button = 2 then (
                    rotate_cursor_img ();
                    Dom.preventDefault ev;
                    Dom_html.stopPropagation ev;
                    Js._false)
                  else Js._true))
             Js._false);
        ignore
          (Dom_html.addEventListener Dom_html.document
             (Dom_html.Event.make "contextmenu")
             (Dom_html.handler (fun ev ->
                  if !cursor_switch then (
                    (* cursor/patch is active → stop menu *)
                    Dom.preventDefault ev;
                    Dom_html.stopPropagation ev;
                    Js._false)
                  else Js._true))
             Js._false);
        (match advance_button with
        | None -> log "Missing advance button."
        | Some btn ->
            btn##.onclick :=
              Dom_html.handler (fun _ ->
                  let current_player = (unwrap_exn !current_state).turn in
                  let old_buttons = current_player.buttons_owned in
                  status_div##.textContent := Js.some (Js.string "Advance!");
                  status_div##.classList##remove (Js.string "hide");
                  status_div##.classList##remove (Js.string "show");
                  ignore status_div##.offsetWidth;
                  status_div##.classList##add (Js.string "show");
                  _delay_ms 2000 (fun () ->
                      status_div##.classList##add (Js.string "hide"));
                  current_state :=
                    Some (Move.choose_move (unwrap_exn !current_state) Advance 0 0 0);
                  show_nameplate_for_turn ();
                  let button_increase_val = current_player.buttons_owned - old_buttons in
                  if current_player.player_num = 1 then (
                    position_token_on_board cube_token 1;
                    update_buttons_ui current_player 1;
                    flash_button_anim "button_up_p1" button_increase_val)
                  else (
                    position_token_on_board pyramid_token 2;
                    update_buttons_ui current_player 2;
                    flash_button_anim "button_up_p2" button_increase_val);
                  let end_game_check, p1s, p2s = check_if_winner () in
                  if end_game_check = 1 then
                    announce_winner 1 (unwrap_exn !current_state).tk1.owned_by.player_name
                      p1s p2s
                  else if end_game_check = 2 then announce_winner 2 "AI" p1s p2s
                  else if end_game_check = 0 then announce_winner 0 "None" p1s p2s;
                  play_ai_turn ();

                  Js._false));
        (match place_patch_button with
        | None -> log "Missing place patch button."
        | Some btn ->
            btn##.onclick :=
              Dom_html.handler (fun _ ->
                  let player_num = (unwrap_exn !current_state).turn.player_num in
                  if player_num = 1 then
                    paint_quilt_board (unwrap_exn !current_state).p1qb
                      quilt_board_replica_id "p1_qb_svg"
                  else
                    paint_quilt_board (unwrap_exn !current_state).p2qb
                      quilt_board_replica_id "p2_qb_svg";
                  patch_interface##.style##.display := Js.string "block";
                  blur##.style##.display := Js.string "block";
                  set_patch_choice_srcs ();

                  Js._false));
        (match choose_patch_1_button with
        | None -> log "Missing choose patch button."
        | Some btn ->
            btn##.onclick :=
              Dom_html.handler (fun _ ->
                  let player_num = (unwrap_exn !current_state).turn.player_num in
                  patch_1_choice_index :=
                    Patch.get_one (unwrap_exn !current_state).neut.pos
                      (unwrap_exn !current_state).patches_remaining
                      (unwrap_exn !current_state).patches_remaining;
                  pending_patch_index := Some !patch_1_choice_index;
                  patch_interface##.style##.display := Js.string "none";
                  blur##.style##.display := Js.string "none";
                  container##.style##.display := Js.string "none";
                  patch_chosen##.style##.display := Js.string "grid";
                  let patch_name =
                    find_patch_name (unwrap_exn !current_state).patches
                      !patch_1_choice_index
                  in
                  let patch_shape =
                    Patch.patch_shape_of_sexp (Sexplib.Sexp.of_string patch_name)
                  in
                  let patch_cols, _ = Patch.get_col_row patch_shape in
                  let img_src = "svgs/" ^ patch_name ^ ".svg" in
                  show_cursor_img ~src:img_src ~cols:patch_cols patch_shape;

                  let grid_cells =
                    Dom_html.document##getElementsByClassName (Js.string "replica-cell")
                  in
                  for i = 0 to grid_cells##.length - 1 do
                    let cell = Js.Opt.get (grid_cells##item i) (fun () -> assert false) in
                    cell##.onclick :=
                      Dom_html.handler (fun event ->
                          let style = Dom_html.window##getComputedStyle cell in
                          let row_opt = css_var_int style "--row" in
                          let col_opt = css_var_int style "--col" in
                          let _target = Dom_html.eventTarget event in
                          match (col_opt, row_opt) with
                          | Some c, Some r ->
                              let column = Js.to_string (Js.string (string_of_int c)) in
                              let row = Js.to_string (Js.string (string_of_int r)) in
                              log ("col clicked: " ^ column ^ ", row clicked: " ^ row);
                              let current_player = (unwrap_exn !current_state).turn in
                              let old_buttons = current_player.buttons_owned in
                              let old_neut_pos = (unwrap_exn !current_state).neut.pos in
                              let updated_state =
                                Move.choose_move (unwrap_exn !current_state) PlacePatch
                                  !patch_1_choice_index (int_of_string row)
                                  (int_of_string column)
                              in
                              current_state := Some updated_state;
                              disable_taken_patch_img !patch_1_choice_index;
                              place_neut_token_on_circle old_neut_pos;
                              patch_chosen##.style##.display := Js.string "none";
                              container##.style##.display := Js.string "grid";
                              blur##.style##.display := Js.string "none";
                              show_nameplate_for_turn ();
                              let new_buttons =
                                current_player.buttons_owned - old_buttons
                              in
                              if player_num = 1 then
                                paint_quilt_board (unwrap_exn !current_state).p1qb
                                  quilt_board_replica_id "p1_qb_svg"
                              else
                                paint_quilt_board (unwrap_exn !current_state).p2qb
                                  quilt_board_replica_id "p2_qb_svg";
                              if current_player.player_num = 1 then (
                                position_token_on_board cube_token 1;
                                update_buttons_ui current_player 1;
                                print_filled_slots (unwrap_exn !current_state).p1qb;
                                flash_button_anim "button_up_p1" new_buttons)
                              else (
                                position_token_on_board pyramid_token 2;
                                update_buttons_ui current_player 2;
                                print_filled_slots (unwrap_exn !current_state).p2qb;
                                flash_button_anim "button_up_p2" new_buttons);
                              hide_cursor_img ();
                              play_ai_turn ();
                              let end_game_check, p1s, p2s = check_if_winner () in
                              if end_game_check = 1 then
                                announce_winner 1
                                  (unwrap_exn !current_state).tk1.owned_by.player_name p1s
                                  p2s
                              else if end_game_check = 2 then
                                announce_winner 2 "AI" p1s p2s
                              else if end_game_check = 0 then
                                announce_winner 0 "None" p1s p2s;
                              Js._false
                          | _ ->
                              ();
                              Js._false)
                  done;

                  Js._true));
        (match choose_patch_2_button with
        | None -> log "Missing choose patch button."
        | Some btn ->
            btn##.onclick :=
              Dom_html.handler (fun _ ->
                  let player_num = (unwrap_exn !current_state).turn.player_num in
                  patch_2_choice_index :=
                    Patch.get_one !patch_1_choice_index
                      (unwrap_exn !current_state).patches_remaining
                      (unwrap_exn !current_state).patches_remaining;
                  pending_patch_index := Some !patch_1_choice_index;
                  patch_interface##.style##.display := Js.string "none";
                  blur##.style##.display := Js.string "none";
                  container##.style##.display := Js.string "none";
                  patch_chosen##.style##.display := Js.string "grid";
                  let patch_name =
                    find_patch_name (unwrap_exn !current_state).patches
                      !patch_2_choice_index
                  in
                  let patch_shape =
                    Patch.patch_shape_of_sexp (Sexplib.Sexp.of_string patch_name)
                  in
                  let patch_cols, _ = Patch.get_col_row patch_shape in
                  let img_src = "svgs/" ^ patch_name ^ ".svg" in
                  show_cursor_img ~src:img_src ~cols:patch_cols patch_shape;

                  let grid_cells =
                    Dom_html.document##getElementsByClassName (Js.string "replica-cell")
                  in
                  for i = 0 to grid_cells##.length - 1 do
                    let cell = Js.Opt.get (grid_cells##item i) (fun () -> assert false) in
                    cell##.onclick :=
                      Dom_html.handler (fun event ->
                          let style = Dom_html.window##getComputedStyle cell in
                          let row_opt = css_var_int style "--row" in
                          let col_opt = css_var_int style "--col" in
                          let _target = Dom_html.eventTarget event in
                          match (col_opt, row_opt) with
                          | Some c, Some r ->
                              let column = Js.to_string (Js.string (string_of_int c)) in
                              let row = Js.to_string (Js.string (string_of_int r)) in
                              log ("col clicked: " ^ column ^ ", row clicked: " ^ row);
                              let current_player = (unwrap_exn !current_state).turn in
                              let old_buttons = current_player.buttons_owned in
                              let old_neut_pos = (unwrap_exn !current_state).neut.pos in
                              let updated_state =
                                Move.choose_move (unwrap_exn !current_state) PlacePatch
                                  !patch_2_choice_index (int_of_string row)
                                  (int_of_string column)
                              in
                              current_state := Some updated_state;
                              disable_taken_patch_img !patch_2_choice_index;
                              place_neut_token_on_circle old_neut_pos;
                              patch_chosen##.style##.display := Js.string "none";
                              container##.style##.display := Js.string "grid";
                              blur##.style##.display := Js.string "none";
                              show_nameplate_for_turn ();
                              if player_num = 1 then
                                paint_quilt_board (unwrap_exn !current_state).p1qb
                                  quilt_board_replica_id "p1_qb_svg"
                              else
                                paint_quilt_board (unwrap_exn !current_state).p2qb
                                  quilt_board_replica_id "p2_qb_svg";
                              let new_buttons =
                                current_player.buttons_owned - old_buttons
                              in
                              if current_player.player_num = 1 then (
                                position_token_on_board cube_token 1;
                                update_buttons_ui current_player 1;
                                print_filled_slots (unwrap_exn !current_state).p1qb;
                                flash_button_anim "button_up_p1" new_buttons)
                              else (
                                position_token_on_board pyramid_token 2;
                                update_buttons_ui current_player 2;
                                print_filled_slots (unwrap_exn !current_state).p2qb;
                                flash_button_anim "button_up_p2" new_buttons);
                              hide_cursor_img ();
                              play_ai_turn ();
                              let end_game_check, p1s, p2s = check_if_winner () in
                              if end_game_check = 1 then
                                announce_winner 1
                                  (unwrap_exn !current_state).tk1.owned_by.player_name p1s
                                  p2s
                              else if end_game_check = 2 then
                                announce_winner 2 "AI" p1s p2s
                              else if end_game_check = 0 then
                                announce_winner 0 "None" p1s p2s;
                              Js._false
                          | _ ->
                              ();
                              Js._false)
                  done;
                  Js._true));
        (match choose_patch_3_button with
        | None -> log "Missing choose patch button."
        | Some btn ->
            btn##.onclick :=
              Dom_html.handler (fun _ ->
                  let player_num = (unwrap_exn !current_state).turn.player_num in
                  patch_3_choice_index :=
                    Patch.get_one !patch_2_choice_index
                      (unwrap_exn !current_state).patches_remaining
                      (unwrap_exn !current_state).patches_remaining;
                  pending_patch_index := Some !patch_3_choice_index;
                  patch_interface##.style##.display := Js.string "none";
                  blur##.style##.display := Js.string "none";
                  container##.style##.display := Js.string "none";
                  patch_chosen##.style##.display := Js.string "grid";
                  let patch_name =
                    find_patch_name (unwrap_exn !current_state).patches
                      !patch_3_choice_index
                  in
                  let patch_shape =
                    Patch.patch_shape_of_sexp (Sexplib.Sexp.of_string patch_name)
                  in
                  let patch_cols, _ = Patch.get_col_row patch_shape in
                  let img_src = "svgs/" ^ patch_name ^ ".svg" in
                  show_cursor_img ~src:img_src ~cols:patch_cols patch_shape;

                  let grid_cells =
                    Dom_html.document##getElementsByClassName (Js.string "replica-cell")
                  in
                  for i = 0 to grid_cells##.length - 1 do
                    let cell = Js.Opt.get (grid_cells##item i) (fun () -> assert false) in
                    cell##.onclick :=
                      Dom_html.handler (fun event ->
                          let style = Dom_html.window##getComputedStyle cell in
                          let row_opt = css_var_int style "--row" in
                          let col_opt = css_var_int style "--col" in
                          let _target = Dom_html.eventTarget event in
                          match (col_opt, row_opt) with
                          | Some c, Some r ->
                              let column = Js.to_string (Js.string (string_of_int c)) in
                              let row = Js.to_string (Js.string (string_of_int r)) in
                              log ("col clicked: " ^ column ^ ", row clicked: " ^ row);
                              let current_player = (unwrap_exn !current_state).turn in
                              let old_buttons = current_player.buttons_owned in
                              let old_neut_pos = (unwrap_exn !current_state).neut.pos in
                              let updated_state =
                                Move.choose_move (unwrap_exn !current_state) PlacePatch
                                  !patch_3_choice_index (int_of_string row)
                                  (int_of_string column)
                              in
                              current_state := Some updated_state;
                              disable_taken_patch_img !patch_3_choice_index;
                              place_neut_token_on_circle old_neut_pos;
                              patch_chosen##.style##.display := Js.string "none";
                              container##.style##.display := Js.string "grid";
                              blur##.style##.display := Js.string "none";
                              show_nameplate_for_turn ();
                              let new_buttons =
                                current_player.buttons_owned - old_buttons
                              in
                              if player_num = 1 then
                                paint_quilt_board (unwrap_exn !current_state).p1qb
                                  quilt_board_replica_id "p1_qb_svg"
                              else
                                paint_quilt_board (unwrap_exn !current_state).p2qb
                                  quilt_board_replica_id "p2_qb_svg";
                              if current_player.player_num = 1 then (
                                position_token_on_board cube_token 1;
                                update_buttons_ui current_player 1;
                                print_filled_slots (unwrap_exn !current_state).p1qb;
                                flash_button_anim "button_up_p1" new_buttons)
                              else (
                                position_token_on_board pyramid_token 2;
                                update_buttons_ui current_player 2;
                                print_filled_slots (unwrap_exn !current_state).p2qb;
                                flash_button_anim "button_up_p2" new_buttons);
                              hide_cursor_img ();
                              _delay_ms 1500 (fun () -> play_ai_turn ());
                              let end_game_check, p1s, p2s = check_if_winner () in
                              if end_game_check = 1 then
                                announce_winner 1
                                  (unwrap_exn !current_state).tk1.owned_by.player_name p1s
                                  p2s
                              else if end_game_check = 2 then
                                announce_winner 2 "AI" p1s p2s
                              else if end_game_check = 0 then
                                announce_winner 0 "None" p1s p2s;
                              Js._false
                          | _ ->
                              ();
                              Js._false)
                  done;
                  Js._true));
        (match cancel_place_patch_button with
        | None -> log "Missing go back button."
        | Some btn ->
            btn##.onclick :=
              Dom_html.handler (fun _ ->
                  hide_cursor_img ();
                  patch_chosen##.style##.display := Js.string "none";
                  container##.style##.display := Js.string "grid";
                  blur##.style##.display := Js.string "none";
                  Js._true));
        (match go_back_button with
        | None -> log "Missing go back button."
        | Some btn ->
            btn##.onclick :=
              Dom_html.handler (fun _ ->
                  patch_interface##.style##.display := Js.string "none";
                  blur##.style##.display := Js.string "none";
                  Js._false));
        (match start_game_button with
        | None -> log "Missing start button."
        | Some btn ->
            btn##.onclick :=
              Dom_html.handler (fun _ ->
                  let raw_input =
                    match player_name with
                    | None ->
                        log "Player name input not found.";
                        "Player"
                    | Some input -> Js.to_string input##.value
                  in

                  let pname = raw_input |> String.trim in
                  if String.length pname > 23 then (
                    let name_error_div = Dom_html.getElementById_exn "name_error" in
                    name_error_div##.textContent := Js.some (Js.string "Name too long.");
                    Js._true)
                  else
                    let pname = if pname = "" then "Player" else pname in
                    let state = initialize_game pname in
                    current_state := Some state;
                    place_patch_images_around_circle state.patches;
                    place_neut_token_on_circle state.neut.pos;
                    position_token_on_board cube_token 1;
                    position_token_on_board pyramid_token 2;
                    let count = 5 in
                    set_button_count_and_src ~count ~txt_count_id:"p1_buttons"
                      ~image_id:"p1_buttons_img";
                    set_button_count_and_src ~count ~txt_count_id:"p2_buttons"
                      ~image_id:"p2_buttons_img";
                    set_player_name pname "player_name_turn_greeting";
                    log "Game started.";
                    intro##.style##.display := Js.string "none";
                    container##.style##.display := Js.string "grid";
                    show_nameplate_for_turn ();

                    Js._false));
        Js._false)
