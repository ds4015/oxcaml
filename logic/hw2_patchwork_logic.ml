open! Core
open Js_of_ocaml

let log s = Firebug.console##log (Js.string s)

(* Players *)

module Player = struct
  type t =
    { player_num : int
    ; player_name : string
    ; mutable buttons_owned : int
    ; mutable score : int
    }
  [@@deriving sexp, compare, equal]
end

(* Patches *)

module Patch = struct
  type patch_shape =
    | Square
    | SquareNub
    | SquareHighFive
    | TCross
    | S
    | LongI
    | LHalfH
    | SHalfH
    | H
    | Corner
    | CornerRev
    | SLVert
    | ShortI
    | LRev
    | LongL
    | L
    | ChunkyLRev
    | SmallI
    | I
    | ShortT
    | StubbyT
    | T
    | Plus
    | Zig
    | ZigZag
    | ZigRev
    | ChunkyZig
    | Cross
    | INub
    | WideStubbyT
    | Prong
    | Vine
    | WidePlus
    | Empty
  [@@deriving sexp, compare, equal]

  type t =
    { shape : patch_shape
    ; cost : int
    ; mutable pos_around_board : int
    ; move_num : int
    ; income : int
    ; mutable rotated : int
    }
  [@@deriving sexp, compare, equal]

  let get_patch_dim p =
    match p with
    | Square -> [ 1, "U"; 1, "R"; 1, "D" ]
    | SquareNub -> [ 1, "U"; 1, "R"; 1, "U"; 1, "SD"; 1, "D" ]
    | SquareHighFive -> [ 1, "U"; 2, "R"; 1, "U"; 1, "SD"; 1, "SL"; 1, "D" ]
    | TCross -> [ 2, "U"; 1, "L"; 1, "SR"; 1, "R"; 1, "SL"; 2, "U" ]
    | S -> [ 1, "R"; 4, "U"; 1, "R" ]
    | LongI -> [ 4, "U" ]
    | LHalfH -> [ 1, "U"; 1, "SD"; 3, "R"; 1, "U" ]
    | SHalfH -> [ 1, "U"; 1, "SD"; 2, "R"; 1, "U" ]
    | H -> [ 2, "U"; 1, "SD"; 2, "R"; 1, "U"; 1, "SD"; 1, "D" ]
    | Corner -> [ 1, "U"; 1, "L" ]
    | CornerRev -> [ 1, "L"; 1, "U" ]
    | SLVert -> [ 2, "U"; 1, "L"; 1, "U" ]
    | ShortI -> [ 2, "U" ]
    | I -> [ 3, "U" ]
    | LRev -> [ 1, "R"; 2, "U" ]
    | LongL -> [ 1, "L"; 3, "U" ]
    | L -> [ 1, "L"; 2, "U" ]
    | ChunkyLRev -> [ 1, "R"; 2, "U"; 1, "SD"; 1, "L" ]
    | SmallI -> [ 1, "U" ]
    | ShortT -> [ 2, "U"; 1, "R"; 1, "SL"; 1, "L" ]
    | StubbyT -> [ 1, "U"; 1, "R"; 1, "SL"; 1, "L" ]
    | T -> [ 3, "U"; 1, "R"; 1, "SL"; 1, "L" ]
    | Plus -> [ 1, "U"; 1, "R"; 1, "SL"; 1, "L"; 1, "SR"; 1, "U" ]
    | Zig -> [ 1, "U"; 1, "R"; 1, "U" ]
    | ZigZag -> [ 1, "U"; 1, "L"; 1, "U"; 1, "L" ]
    | ZigRev -> [ 1, "U"; 1, "L"; 1, "U" ]
    | ChunkyZig -> [ 2, "U"; 1, "R"; 1, "U"; 1, "SD"; 1, "D" ]
    | Cross -> [ 2, "U"; 1, "R"; 1, "SL"; 1, "L"; 1, "SR"; 2, "U" ]
    | INub -> [ 1, "U"; 1, "L"; 1, "SR"; 2, "U" ]
    | WideStubbyT -> [ 1, "R"; 1, "U"; 1, "R"; 1, "SL"; 2, "L" ]
    | Prong -> [ 1, "U"; 1, "R"; 1, "U"; 1, "SD"; 1, "R"; 1, "D" ]
    | Vine -> [ 1, "U"; 1, "R"; 1, "SL"; 1, "U"; 1, "L"; 1, "SR"; 2, "U" ]
    | WidePlus ->
      [ 1, "U"; 1, "L"; 1, "SR"; 1, "U"; 1, "R"; 1, "D"; 1, "R"; 1, "SL"; 1, "D" ]
    | Empty -> []
  ;;

  let rotate (p : t) =
    if p.rotated = 0
    then (
      match p.shape with
      | Square -> [ 2, "R"; 1, "D"; 1, "L" ]
      | SquareNub -> [ 2, "L"; 1, "U"; 1, "L"; 1, "D" ]
      | SquareHighFive -> [ 2, "L"; 2, "U"; 1, "L"; 1, "D" ]
      | TCross -> [ 1, "D"; 2, "L"; 2, "SR"; 1, "R"; 1, "SL"; 1, "D" ]
      | S -> [ 2, "D"; 2, "R"; 1, "D" ]
      | LongI -> [ 5, "R" ]
      | LHalfH -> [ 2, "L"; 3, "D"; 1, "R" ]
      | SHalfH -> [ 2, "L"; 2, "D"; 1, "R" ]
      | H -> [ 3, "R"; 1, "SL"; 2, "D"; 1, "L"; 1, "SR"; 1, "R" ]
      | Corner -> [ 2, "R"; 1, "U" ]
      | CornerRev -> [ 2, "L"; 1, "D" ]
      | SLVert -> [ 2, "L"; 1, "D"; 2, "L" ]
      | ShortI -> [ 3, "R" ]
      | I -> [ 4, "R" ]
      | LRev -> [ 1, "R"; 2, "U" ]
      | LongL -> [ 4, "L"; 1, "D" ]
      | L -> [ 3, "L"; 1, "D" ]
      | ChunkyLRev -> [ 1, "D"; 1, "R"; 2, "U" ]
      | SmallI -> [ 2, "R" ]
      | ShortT -> [ 2, "D"; 1, "L"; 1, "SR"; 1, "D" ]
      | StubbyT -> [ 2, "D"; 2, "L"; 2, "SR"; 1, "D" ]
      | T -> [ 2, "D"; 3, "L"; 3, "SR"; 1, "D" ]
      | Plus -> [ 2, "D"; 1, "L"; 1, "SR"; 1, "R"; 1, "SL"; 1, "D" ]
      | Zig -> [ 1, "U"; 1, "SD"; 1, "R"; 1, "D" ]
      | ZigZag -> [ 2, "D"; 1, "L"; 1, "D"; 1, "L" ]
      | ZigRev -> [ 2, "R"; 1, "D"; 1, "R" ]
      | ChunkyZig -> [ 3, "R"; 1, "D"; 2, "R" ]
      | Cross -> [ 2, "D"; 1, "R"; 1, "SL"; 2, "L"; 2, "SR"; 1, "D" ]
      | INub -> [ 1, "R"; 1, "D"; 1, "SU"; 3, "U" ]
      | WideStubbyT -> [ 2, "R"; 1, "D"; 1, "SU"; 1, "R" ]
      | Prong -> [ 2, "R"; 1, "D"; 1, "R"; 1, "SL"; 1, "D"; 1, "L" ]
      | Vine -> [ 2, "D"; 1, "R"; 1, "SL"; 2, "L"; 1, "SR"; 1, "D" ]
      | WidePlus -> [ 2, "D"; 1, "L"; 1, "D"; 1, "R"; 1, "D"; 1, "SU"; 1, "R"; 1, "U" ]
      | Empty -> [])
    else get_patch_dim p.shape
  ;;

  let get_values p =
    match p with
    | Square -> 6, 5
    | SquareNub -> 2, 2
    | SquareHighFive -> 8, 6
    | TCross -> 1, 4
    | S -> 1, 2
    | LongI -> 7, 1
    | LHalfH -> 1, 5
    | SHalfH -> 1, 2
    | H -> 2, 3
    | Corner -> 3, 1
    | CornerRev -> 1, 3
    | SLVert -> 2, 3
    | ShortI -> 2, 2
    | LRev -> 4, 2
    | LongL -> 10, 3
    | L -> 4, 6
    | ChunkyLRev -> 10, 5
    | SmallI -> 2, 1
    | I -> 3, 3
    | ShortT -> 2, 2
    | StubbyT -> 5, 5
    | T -> 7, 2
    | Plus -> 5, 4
    | Zig -> 3, 2
    | ZigZag -> 10, 4
    | ZigRev -> 7, 6
    | ChunkyZig -> 4, 2
    | Cross -> 0, 3
    | INub -> 3, 4
    | WideStubbyT -> 7, 4
    | Prong -> 3, 6
    | Vine -> 2, 1
    | WidePlus -> 5, 3
    | Empty -> 0, 0
  ;;

  let get_income p =
    match p with
    | Square -> 2
    | SquareNub -> 0
    | SquareHighFive -> 3
    | TCross -> 1
    | S -> 0
    | LongI -> 1
    | LHalfH -> 1
    | SHalfH -> 0
    | H -> 0
    | Corner -> 0
    | CornerRev -> 0
    | SLVert -> 1
    | ShortI -> 0
    | LRev -> 1
    | LongL -> 2
    | L -> 2
    | ChunkyLRev -> 3
    | SmallI -> 0
    | I -> 1
    | ShortT -> 0
    | StubbyT -> 2
    | T -> 2
    | Plus -> 2
    | Zig -> 1
    | ZigZag -> 3
    | ZigRev -> 3
    | ChunkyZig -> 0
    | Cross -> 1
    | INub -> 1
    | WideStubbyT -> 2
    | Prong -> 2
    | Vine -> 0
    | WidePlus -> 1
    | Empty -> 0
  ;;

  let get_col_row p =
    match p with
    | Square -> 2, 2
    | SquareNub -> 2, 3
    | SquareHighFive -> 3, 3
    | TCross -> 3, 5
    | S -> 3, 5
    | LongI -> 1, 5
    | LHalfH -> 4, 2
    | SHalfH -> 3, 2
    | H -> 3, 3
    | Corner -> 2, 2
    | CornerRev -> 2, 2
    | SLVert -> 2, 4
    | ShortI -> 1, 3
    | LRev -> 2, 3
    | LongL -> 2, 4
    | L -> 2, 3
    | ChunkyLRev -> 2, 4
    | SmallI -> 1, 2
    | I -> 1, 4
    | ShortT -> 3, 2
    | StubbyT -> 3, 3
    | T -> 3, 4
    | Plus -> 3, 3
    | Zig -> 2, 3
    | ZigZag -> 3, 3
    | ZigRev -> 2, 4
    | ChunkyZig -> 2, 4
    | Cross -> 3, 5
    | INub -> 2, 5
    | WideStubbyT -> 2, 4
    | Prong -> 3, 4
    | Vine -> 3, 4
    | WidePlus -> 3, 3
    | Empty -> 0, 0
  ;;

  let shapes =
    [ Square
    ; SquareNub
    ; SquareHighFive
    ; TCross
    ; S
    ; LongI
    ; LHalfH
    ; SHalfH
    ; H
    ; Corner
    ; CornerRev
    ; SLVert
    ; ShortI
    ; LRev
    ; LongL
    ; L
    ; ChunkyLRev
    ; SmallI
    ; I
    ; ShortT
    ; StubbyT
    ; T
    ; Plus
    ; Zig
    ; ZigZag
    ; ZigRev
    ; ChunkyZig
    ; Cross
    ; INub
    ; WideStubbyT
    ; Prong
    ; Vine
    ; WidePlus
    ]
  ;;

  let get_area patch =
    match patch with
    | Square -> 4
    | SquareNub -> 5
    | SquareHighFive -> 6
    | TCross -> 6
    | S -> 7
    | LongI -> 5
    | LHalfH -> 6
    | SHalfH -> 5
    | H -> 7
    | Corner -> 3
    | CornerRev -> 3
    | SLVert -> 5
    | ShortI -> 3
    | LRev -> 4
    | LongL -> 5
    | L -> 4
    | ChunkyLRev -> 6
    | SmallI -> 2
    | I -> 4
    | ShortT -> 4
    | StubbyT -> 5
    | T -> 6
    | Plus -> 5
    | Zig -> 4
    | ZigZag -> 5
    | ZigRev -> 4
    | ChunkyZig -> 6
    | Cross -> 7
    | INub -> 6
    | WideStubbyT -> 6
    | Prong -> 7
    | Vine -> 6
    | WidePlus -> 8
    | Empty -> 0
  ;;

  let rec index_to_patch (patches : t list) i =
    match patches with
    | [] ->
      { shape = Empty
      ; pos_around_board = -1
      ; cost = 0
      ; move_num = 0
      ; income = 0
      ; rotated = 0
      }
    | hd :: tl -> if hd.pos_around_board = i then hd else index_to_patch tl i
  ;;

  exception No_patches_left

  let rotate_dir dir =
    match dir with
    | "D" -> "L"
    | "L" -> "U"
    | "U" -> "R"
    | "R" -> "D"
    | "SD" -> "SL"
    | "SL" -> "SU"
    | "SU" -> "SR"
    | "SR" -> "SD"
    | _ -> dir
  ;;

  let get_rotated_dim (dim : (int * string) list) =
    let rec loop dims acc =
      match dims with
      | [] -> List.rev acc
      | (num, dir) :: tl ->
        let new_dir = rotate_dir dir in
        loop tl ((num, new_dir) :: acc)
    in
    loop dim []
  ;;

  let shuffle_patches (pl : t list) : t list =
    let arr = Array.of_list pl in
    let num_patches = Array.length arr in
    for i = num_patches - 1 downto 1 do
      let j = Random.int (i + 1) in
      let temp = arr.(i) in
      let temp_pos = arr.(i).pos_around_board in
      arr.(i).pos_around_board <- arr.(j).pos_around_board;
      arr.(i) <- arr.(j);
      arr.(j).pos_around_board <- temp_pos;
      arr.(j) <- temp
    done;
    Array.to_list arr
  ;;

  let rec get_one i rem_list orig_rl =
    let start_over rl =
      match rl with
      | [] -> raise No_patches_left
      | hd :: _ -> hd
    in
    match rem_list with
    | hd :: tl -> if hd > i then hd else get_one i tl orig_rl
    | [] -> start_over orig_rl
  ;;

  let rec build_patch_set shapes (patches : t list) acc =
    match shapes with
    | [] -> List.rev patches
    | hd :: t ->
      let patch_attr = get_values hd in
      let patch_inc = get_income hd in
      let patch =
        { shape = hd
        ; pos_around_board = acc
        ; cost = fst patch_attr
        ; move_num = snd patch_attr
        ; income = patch_inc
        ; rotated = 0
        }
      in
      let pl_updated = patch :: patches in
      build_patch_set t pl_updated (acc + 1)
  ;;

  let init_patches () =
    Random.self_init ();
    let patch_list = build_patch_set shapes [] 1 in
    shuffle_patches patch_list
  ;;

  let find_initial_neut_pos (pl : t list) : int =
    let rec iter_patch_list = function
      | [] -> 1
      | (p : t) :: tl ->
        (match p.shape with
         | Corner -> p.pos_around_board
         | _ -> iter_patch_list tl)
    in
    iter_patch_list pl
  ;;
end

(* Game Boards *)

module Game_board = struct
  type main_board =
    { squares : int
    ; special_patch_locs : int list
    }
  [@@deriving sexp, compare, equal]

  type quilt_board =
    { squares : int
    ; filled_squares : (int * int) list
    ; accumulated_income : int
    ; patches : (int * int * Patch.patch_shape) list
    }
  [@@deriving sexp, compare, equal]

  type t =
    | MainBoard of main_board
    | QuiltBoard of quilt_board
  [@@deriving sexp, compare, equal]

  exception Out_of_bounds
  exception Patch_does_not_fit_there

  let print_filled_slots (board : quilt_board) =
    let filled = board.filled_squares in
    let rec iter f =
      match f with
      | [] ->
        log "Done";
        ()
      | hd :: tl ->
        log (string_of_int (fst hd) ^ ", " ^ string_of_int (snd hd));
        iter tl
    in
    iter filled
  ;;

  let calc_income (b : quilt_board) old_pos new_pos =
    let board_income = b.accumulated_income in
    let board_button_locs = [ 6; 12; 18; 24; 30; 36; 42; 48; 54 ] in
    let rec check bbl acc =
      match bbl with
      | [] -> acc
      | hd :: tl ->
        if old_pos < hd && new_pos >= hd
        then check tl (acc + board_income)
        else check tl acc
    in
    check board_button_locs board_income
  ;;

  (* let rec check_patch_squares (f : (int * int) list) qb sr sc dir acc = if acc < 1 then
     (sr, sc) else let rec check_all_filled (filled_squares : (int * int) list) : bool =
     match filled_squares with | [] -> true | (r, c) :: tl -> if r = sr && c = sc then
     raise Patch_does_not_fit_there else check_all_filled tl in

     (* current square not in filled *) if check_all_filled f then match dir with | "D" ->
     if sr < qb.squares then check_patch_squares f qb (sr + 1) sc dir (acc - 1) else raise
     Out_of_bounds | "U" -> if sr > 1 then check_patch_squares f qb (sr - 1) sc dir (acc -
     1) else raise Out_of_bounds | "L" -> if sc > 1 then check_patch_squares f qb sr (sc -
     1) dir (acc - 1) else raise Out_of_bounds | "R" -> if sc < qb.squares then
     check_patch_squares f qb sr (sc + 1) dir (acc - 1) else raise Out_of_bounds | "SU" ->
     (sr - 1, sc) | "SD" -> (sr + 1, sc) | "SL" -> (sr, sc - 1) | "SR" -> (sr, sc + 1) | _
     -> (-1, -1) else (-1, -1) *)

  let check_if_patch_fits dim board start_row start_col =
    let rec is_square_in_filled_squares f r c =
      match f with
      | [] -> false
      | hd :: tl ->
        if r = fst hd && c = snd hd then true else is_square_in_filled_squares tl r c
    in
    let rec check_each_patch_cell dim r c =
      let row = ref r in
      let col = ref c in
      let does_it_fit = ref true in
      match dim with
      | [] -> !does_it_fit
      | (num, dir) :: tl ->
        for _i = 0 to num - 1 do
          if !does_it_fit
          then (
            (match dir with
             | "D" ->
               row := !row + 1;
               if !row > 9 || !row < 1 then raise Out_of_bounds else ()
             | "U" ->
               row := !row - 1;
               if !row > 9 || !row < 1 then raise Out_of_bounds else ()
             | "L" ->
               col := !col - 1;
               if !col > 9 || !col < 1 then raise Out_of_bounds else ()
             | "R" ->
               col := !col + 1;
               if !col > 9 || !col < 1 then raise Out_of_bounds else ()
             | "SU" -> row := !row - 1
             | "SL" -> col := !col - 1
             | "SR" -> col := !col + 1
             | "SD" -> row := !row + 1
             | _ -> ());
            if is_square_in_filled_squares board.filled_squares !row !col
            then does_it_fit := false)
        done;
        if !does_it_fit then check_each_patch_cell tl !row !col else false
    in
    if is_square_in_filled_squares board.filled_squares start_row start_col
    then false
    else check_each_patch_cell dim start_row start_col
  ;;

  (* print_string "Checking if patch fits...\n Current filled slots:\n";
     print_filled_slots board; let process_dir num dir acc = let rec loop i r c acc = if i
     = 0 then acc else match dir with | "D" -> print_string ("checking " ^ string_of_int
     (r + 1) ^ ", " ^ string_of_int col); loop (i - 1) (r + 1) c ((r + 1, c) :: acc) | "U"
     -> print_string ("checking " ^ string_of_int (r - 1) ^ ", " ^ string_of_int col);
     loop (i - 1) (r - 1) c ((r - 1, c) :: acc) | "L" -> print_string ("checking " ^
     string_of_int row ^ ", " ^ string_of_int (c - 1)); loop (i - 1) r (c - 1) ((r, c - 1)
     :: acc) | "R" -> print_string ("checking " ^ string_of_int row ^ ", " ^ string_of_int
     (c + 1)); loop (i - 1) r (c + 1) ((r, c + 1) :: acc) | "SL" -> acc | "SU" -> acc |
     "SR" -> acc | "SD" -> acc | _ -> failwith "Invalid dir" in loop num row col acc in
     let rec process_dims dims acc = match dims with | [] -> acc | (n, d) :: tl -> let
     acc' = process_dir n d acc in process_dims tl acc' in

     let rec check_filled f slot = let r, c = slot in match f with | [] -> true | (row,
     col) :: tl -> if r = row && c = col then false else check_filled tl slot in

     let squares_to_be_filled_with_placement = process_dims dim [ (row, col) ] in let
     filled = board.filled_squares in let rec iter_tbf to_be_filled = match to_be_filled
     with | [] -> true | hd :: tl -> print_string ("fill row: " ^ string_of_int (fst hd) ^
     ", fill col: " ^ string_of_int (snd hd) ^ "\n"); if not (check_filled filled hd) then
     false else iter_tbf tl in print_string "Squares to be filled...\n"; iter_tbf
     squares_to_be_filled_with_placement *)

  (* let rec check_if_patch_fits patchDim board row col = match patchDim with | [] -> true
     | hd :: tl -> let q, d = hd in let upd_row, upd_col = check_patch_squares
     board.filled_squares board row col d q in if upd_row = -1 && upd_col = -1 then false
     else check_if_patch_fits tl board upd_row upd_col *)

  let place_patch_on_quilt_board board patch r c rot =
    let orig_dim = Patch.get_patch_dim patch in
    let rec rotate dim n =
      if n > 0
      then (
        let new_dim = Patch.get_rotated_dim dim in
        rotate new_dim (n - 1))
      else dim
    in
    let dim = rotate orig_dim rot in
    if check_if_patch_fits dim board r c
    then (
      let rec fill_in_new_patch dim cur_r cur_c acc =
        match dim with
        | [] -> acc
        | (num, dir) :: tl ->
          let rec walk n r c acc =
            if n = 0
            then r, c, acc
            else (
              match dir with
              | "D" ->
                let r', c' = r + 1, c in
                walk (n - 1) r' c' ((r', c') :: acc)
              | "U" ->
                let r', c' = r - 1, c in
                walk (n - 1) r' c' ((r', c') :: acc)
              | "L" ->
                let r', c' = r, c - 1 in
                walk (n - 1) r' c' ((r', c') :: acc)
              | "R" ->
                let r', c' = r, c + 1 in
                walk (n - 1) r' c' ((r', c') :: acc)
              | "SD" ->
                let r', c' = r + 1, c in
                r', c', acc
              | "SU" ->
                let r', c' = r - 1, c in
                r', c', acc
              | "SL" ->
                let r', c' = r, c - 1 in
                r', c', acc
              | "SR" ->
                let r', c' = r, c + 1 in
                r', c', acc
              | _ -> r, c, acc)
          in
          let new_r, new_c, acc' = walk num cur_r cur_c acc in
          fill_in_new_patch tl new_r new_c acc'
      in
      let new_filled = fill_in_new_patch dim r c ((r, c) :: board.filled_squares) in
      let old_pip = board.patches in
      let new_pip = (r, c, patch) :: old_pip in
      let new_acc_income = board.accumulated_income + Patch.get_income patch in
      let qb_upd =
        { board with
          filled_squares = new_filled
        ; patches = new_pip
        ; accumulated_income = new_acc_income
        }
      in
      qb_upd)
    else raise Patch_does_not_fit_there
  ;;
end

(* Buttons *)

module Button = struct
  type t = { mutable unassigned_cache : int } [@@deriving sexp, compare, equal]

  exception Insufficient_cache
  exception Insufficient_funds

  let give_buttons b (p : Player.t) n =
    if b.unassigned_cache < n
    then raise Insufficient_cache
    else (
      p.buttons_owned <- p.buttons_owned + n;
      b.unassigned_cache <- b.unassigned_cache - n)
  ;;

  let take_buttons b (p : Player.t) n =
    if p.buttons_owned < n
    then raise Insufficient_funds
    else p.buttons_owned <- p.buttons_owned - n;
    b.unassigned_cache <- b.unassigned_cache + n
  ;;
end

(* Tokens *)

module Token = struct
  type time_token =
    { position : int
    ; owned_by : Player.t
    ; color : string
    }
  [@@deriving sexp, compare, equal]

  type neutral_token = { pos : int } [@@deriving sexp, compare, equal]

  type t =
    | TimeToken of time_token
    | NeutralToken of neutral_token
  [@@deriving sexp, compare, equal]

  let move_token b (t : time_token) (opp : time_token) (board : Game_board.quilt_board) =
    let opp_pos = opp.position in
    let curr_pos = t.position in
    let distance = abs (opp_pos - curr_pos) in
    Button.give_buttons b t.owned_by (distance + 1);
    let new_pos = if opp_pos + 1 >= 54 then 54 else opp_pos + 1 in
    let new_token = { t with position = new_pos } in
    let income_to_add = Game_board.calc_income board curr_pos new_pos in
    Button.give_buttons b t.owned_by income_to_add;
    new_token
  ;;

  let move_token_after_patch b t n board =
    let new_pos = if t.position + n >= 54 then 54 else t.position + n in
    let new_token = { t with position = new_pos } in
    let income_to_add = Game_board.calc_income board t.position new_pos in
    Button.give_buttons b t.owned_by income_to_add;
    new_token
  ;;

  let move_neut_token n = { pos = n }
end

(* Game Pieces *)

module Game_pieces = struct
  type t =
    { player1 : Player.t
    ; player2 : Player.t
    ; time_piece1 : Token.time_token
    ; time_piece2 : Token.time_token
    ; neutral_piece : Token.neutral_token
    ; patch_pieces : Patch.t list
    ; patches_remaining : int list
    ; main_board : Game_board.main_board
    ; quilt_board1 : Game_board.quilt_board
    ; quilt_board2 : Game_board.quilt_board
    ; buttons : Button.t
    }
  [@@deriving sexp, compare, equal]

  let setup_game p1_name p2_name color1 color2 =
    let remaining =
      [ 1
      ; 2
      ; 3
      ; 4
      ; 5
      ; 6
      ; 7
      ; 8
      ; 9
      ; 10
      ; 11
      ; 12
      ; 13
      ; 14
      ; 15
      ; 16
      ; 17
      ; 18
      ; 19
      ; 20
      ; 21
      ; 22
      ; 23
      ; 24
      ; 25
      ; 26
      ; 27
      ; 28
      ; 29
      ; 30
      ; 31
      ; 32
      ; 33
      ]
    in
    let player_1 =
      { Player.player_num = 1
      ; Player.player_name = p1_name
      ; Player.buttons_owned = 5
      ; Player.score = 0
      }
    in
    let player_2 =
      { Player.player_num = 2
      ; Player.player_name = p2_name
      ; Player.buttons_owned = 5
      ; Player.score = 0
      }
    in
    let time_piece_1 : Token.time_token =
      { position = 1; owned_by = player_1; color = color1 }
    in
    let time_piece_2 : Token.time_token =
      { position = 1; owned_by = player_2; color = color2 }
    in
    let patches : Patch.t list = Patch.init_patches () in
    let neut_pos = Patch.find_initial_neut_pos patches in
    let neutral = { Token.pos = neut_pos } in
    let main_board : Game_board.main_board = { squares = 64; special_patch_locs = [] } in
    let quilt_board_1 : Game_board.quilt_board =
      { squares = 9; filled_squares = []; patches = []; accumulated_income = 0 }
    in
    let quilt_board_2 : Game_board.quilt_board =
      { squares = 9; filled_squares = []; patches = []; accumulated_income = 0 }
    in
    let b : Button.t = { unassigned_cache = 500 } in
    let game_pieces =
      { player1 = player_1
      ; player2 = player_2
      ; time_piece1 = time_piece_1
      ; time_piece2 = time_piece_2
      ; neutral_piece = neutral
      ; patch_pieces = patches
      ; patches_remaining = remaining
      ; main_board
      ; quilt_board1 = quilt_board_1
      ; quilt_board2 = quilt_board_2
      ; buttons = b
      }
    in
    game_pieces
  ;;
end

(* Game State *)

module Game_state = struct
  type t =
    { mb : Game_board.main_board
    ; p1qb : Game_board.quilt_board
    ; p2qb : Game_board.quilt_board
    ; bc : Button.t
    ; turn : Player.t
    ; tk1 : Token.time_token
    ; tk2 : Token.time_token
    ; neut : Token.neutral_token
    ; mutable patches : Patch.t list
    ; patches_remaining : int list
    }
  [@@deriving sexp, compare, equal]

  let update st qb1 qb2 bcache turn tt1 tt2 neut p rem =
    { st with
      p1qb = qb1
    ; p2qb = qb2
    ; bc = bcache
    ; turn
    ; tk1 = tt1
    ; tk2 = tt2
    ; neut
    ; patches = p
    ; patches_remaining = rem
    }
  ;;

  let initialize_state (pieces : Game_pieces.t) =
    { bc = pieces.buttons
    ; mb = pieces.main_board
    ; neut = pieces.neutral_piece
    ; p1qb = pieces.quilt_board1
    ; p2qb = pieces.quilt_board2
    ; patches = pieces.patch_pieces
    ; patches_remaining = pieces.patches_remaining
    ; tk1 = pieces.time_piece1
    ; tk2 = pieces.time_piece2
    ; turn = pieces.player1
    }
  ;;
end

(* Move *)

module Move = struct
  type t =
    | Advance
    | PlacePatch

  let advance_on_board b p1t p2t board = Token.move_token b p1t p2t board

  exception No_patches_left
  exception Patch_already_taken

  let rec pl_remove_at i pl =
    match pl with
    | [] -> []
    | (h : Patch.t) :: t when h.pos_around_board = i -> t
    | h :: t -> h :: pl_remove_at i t
  ;;

  let rec reml_remove_at i rem_list =
    match rem_list with
    | [] -> []
    | h :: t when h = i -> t
    | h :: t -> h :: reml_remove_at i t
  ;;

  let rec take_patch (pl : Patch.t list) choice =
    match pl with
    | [] -> raise No_patches_left
    | hd :: tl ->
      if hd.pos_around_board = choice
      then (
        match hd.shape with
        | Empty -> raise Patch_already_taken
        | _ -> hd)
      else take_patch tl choice
  ;;

  (* exception Invalid_patch_choice let choose_patch choice c1 c2 c3 = match choice with |
     1 -> c1 | 2 -> c2 | 3 -> c3 | _ -> raise Invalid_patch_choice*)

  let choose_move state mv patch_choice r c =
    let player_moving = state.Game_state.turn in
    let player = player_moving.player_num in
    log ("Player moving: " ^ string_of_int player);
    let p1t = state.Game_state.tk1 in
    let p2t = state.Game_state.tk2 in
    let pqb = if player = 1 then state.p1qb else state.p2qb in
    let patches = state.patches in
    let remaining_patches = state.patches_remaining in
    let neut = state.neut in
    match mv with
    | Advance ->
      let new_token =
        advance_on_board
          state.Game_state.bc
          (if player = 1 then p1t else p2t)
          (if player = 1 then p2t else p1t)
          pqb
      in
      let next_turn =
        if player = 1
        then
          if new_token.position < p2t.position
          then p1t.owned_by
          else if p2t.position < new_token.position
          then p2t.owned_by
          else p1t.owned_by
        else if new_token.position < p1t.position
        then p2t.owned_by
        else if p1t.position < new_token.position
        then p1t.owned_by
        else p2t.owned_by
      in
      if player = 1
      then (
        let upd_st =
          Game_state.update
            state
            state.p1qb
            state.p2qb
            state.bc
            next_turn
            new_token
            p2t
            neut
            state.patches
            state.patches_remaining
        in
        upd_st)
      else (
        let upd_st =
          Game_state.update
            state
            state.p1qb
            state.p2qb
            state.bc
            next_turn
            p1t
            new_token
            neut
            state.patches
            state.patches_remaining
        in
        upd_st)
    | PlacePatch ->
      let p = take_patch patches patch_choice in
      let pps = pl_remove_at patch_choice patches in
      let rot = p.rotated in
      let upd_rem_list = reml_remove_at patch_choice remaining_patches in
      let qb = Game_board.place_patch_on_quilt_board pqb p.shape r c rot in
      Button.take_buttons state.bc player_moving p.cost;
      let new_token =
        Token.move_token_after_patch
          state.bc
          (if player = 1 then p1t else p2t)
          p.move_num
          qb
      in
      let next_turn =
        if player = 1
        then
          if new_token.position < p2t.position
          then p1t.owned_by
          else if p2t.position < new_token.position
          then p2t.owned_by
          else p1t.owned_by
        else if new_token.position < p1t.position
        then p2t.owned_by
        else if p1t.position < new_token.position
        then p1t.owned_by
        else p2t.owned_by
      in
      let updated_neut = Token.move_neut_token p.pos_around_board in
      log ("Next turn: " ^ string_of_int next_turn.player_num);
      let upd_state =
        if player = 1
        then
          Game_state.update
            state
            qb
            state.p2qb
            state.bc
            next_turn
            new_token
            p2t
            updated_neut
            pps
            upd_rem_list
        else
          Game_state.update
            state
            state.p1qb
            qb
            state.bc
            next_turn
            p1t
            new_token
            updated_neut
            pps
            upd_rem_list
      in
      if player = 1
      then Game_board.print_filled_slots upd_state.p1qb
      else Game_board.print_filled_slots upd_state.p2qb;
      upd_state
  ;;

  let score_game (state : Game_state.t) =
    if state.tk1.position < 54 || state.tk2.position < 54
    then -1, -1
    else (
      let p1_buttons = state.tk1.owned_by.buttons_owned in
      let p2_buttons = state.tk2.owned_by.buttons_owned in
      let p1_empty_squares = 81 - List.length state.p1qb.filled_squares in
      let p2_empty_squares = 81 - List.length state.p2qb.filled_squares in
      p1_buttons - (2 * p1_empty_squares), p2_buttons - (2 * p2_empty_squares))
  ;;

  let check_advance_move_valid (opp_tt : Token.time_token) pos =
    if pos = opp_tt.position + 1 then true else false
  ;;
end
