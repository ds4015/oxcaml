open! Core
open Hw2_patchwork_logic
open Js_of_ocaml
module C = Js_of_ocaml.Firebug

let log s = C.console##log (Js.string s)

let test_board_cells patch (board : Game_board.quilt_board) =
  let valid_cells = ref [] in
  for x = 1 to board.squares do
    for y = 1 to board.squares do
      let valid_cell =
        try Game_board.check_if_patch_fits (Patch.get_patch_dim patch) board x y with
        | _ -> false
      in
      if valid_cell then valid_cells := (x, y) :: !valid_cells else ()
    done
  done;
  !valid_cells
;;

let check_for_holes (board : Game_board.quilt_board) patch cell_row cell_col =
  let adj_empties = ref [] in
  let patch_dim = Patch.get_patch_dim patch in
  let rec cell_filled_or_oob r c filled =
    if r > 9 || r < 1 || c > 9 || c < 1
    then true
    else (
      match filled with
      | [] -> false
      | (rf, cf) :: tl -> if r = rf && c = cf then true else cell_filled_or_oob r c tl)
  in
  (* 1. build adjacent empties list *)
  let build_adjacent_empties_list r c =
    let rec walk_patch dim =
      let last_row = ref r in
      let last_col = ref c in
      match dim with
      | [] -> ()
      | (num, dir) :: tl ->
        for _i = 0 to num - 1 do
          match dir with
          | "U" ->
            last_row := !last_row - 1;
            if not (cell_filled_or_oob (!last_row - 1) !last_col board.filled_squares)
            then adj_empties := (!last_row - 1, !last_col) :: !adj_empties
            else ()
          | "D" ->
            last_row := !last_row + 1;
            if not (cell_filled_or_oob (!last_row + 1) !last_col board.filled_squares)
            then adj_empties := (!last_row + 1, !last_col) :: !adj_empties
            else ()
          | "L" ->
            last_col := !last_col - 1;
            if not (cell_filled_or_oob !last_row (!last_col - 1) board.filled_squares)
            then adj_empties := (!last_row, !last_col - 1) :: !adj_empties
            else ()
          | "R" ->
            last_col := !last_col + 1;
            if not (cell_filled_or_oob !last_row (!last_col + 1) board.filled_squares)
            then adj_empties := (!last_row, !last_col + 1) :: !adj_empties
            else ()
          | "SU" -> last_row := !last_row - 1
          | "SD" -> last_row := !last_row + 1
          | "SL" -> last_col := !last_col - 1
          | "SR" -> last_col := !last_col + 1
          | _ -> ()
        done;
        walk_patch tl
    in
    walk_patch patch_dim
  in
  (* 2. go through adj empties and check all 4 directions for holes *)
  let rec walk_adj_empties adj_e acc =
    match adj_e with
    | [] -> acc
    | (re, ce) :: tl ->
      let up = re - 1, ce in
      let down = re + 1, ce in
      let left = re, ce - 1 in
      let right = re, ce + 1 in
      let up_occ_or_oob = cell_filled_or_oob (fst up) (snd up) board.filled_squares in
      let down_occ_or_oob =
        cell_filled_or_oob (fst down) (snd down) board.filled_squares
      in
      let left_occ_or_oob =
        cell_filled_or_oob (fst left) (snd left) board.filled_squares
      in
      let right_occ_or_oob =
        cell_filled_or_oob (fst right) (snd right) board.filled_squares
      in
      if up_occ_or_oob && down_occ_or_oob && left_occ_or_oob && right_occ_or_oob
      then walk_adj_empties tl (acc + 1)
      else walk_adj_empties tl acc
  in
  build_adjacent_empties_list cell_row cell_col;
  walk_adj_empties !adj_empties 0
;;

let best_score_for_patch
      (board : Game_board.quilt_board)
      (patch_shape : Patch.patch_shape)
      (patch_cost : int)
      (patch_time : int)
      (patch_income : int)
      (extra_turn : float)
      (valid_cells : (int * int) list)
  : float * (int * int)
  =
  let rec loop (cells : (int * int) list) (best_score : float) (best_cell : int * int)
    : float * (int * int)
    =
    match cells with
    | [] -> best_score, best_cell
    | (r, c) :: tl ->
      let holes = check_for_holes board patch_shape r c in
      let score : float =
        float_of_int (Patch.get_area patch_shape)
        +. float_of_int patch_income
        -. (0.8 *. float_of_int patch_time)
        -. (0.3 *. float_of_int patch_cost)
        +. extra_turn
        -. float_of_int holes
      in
      if Float.(score > best_score)
      then loop tl score (r, c)
      else loop tl best_score best_cell
  in
  loop valid_cells (-1e9) (1, 1)
;;

let determine_move (current_state : Game_state.t) =
  log "DETERMINE MOVE TRIGGERED";
  (* get vars needed *)
  let player = current_state.tk1.owned_by in
  let ai = current_state.tk2.owned_by in
  let ai_buttons = ai.buttons_owned in
  let player_buttons = player.buttons_owned in
  let player_position = current_state.tk1.position in
  let ai_position = current_state.tk2.position in
  let neutral_token_pos = current_state.neut.pos in
  let remaining_patches = current_state.patches_remaining in
  let patch_1_ind = Patch.get_one neutral_token_pos remaining_patches remaining_patches in
  let patch_2_ind = Patch.get_one patch_1_ind remaining_patches remaining_patches in
  let patch_3_ind = Patch.get_one patch_2_ind remaining_patches remaining_patches in
  let patch_1 = Patch.index_to_patch current_state.patches patch_1_ind in
  let patch_2 = Patch.index_to_patch current_state.patches patch_2_ind in
  let patch_3 = Patch.index_to_patch current_state.patches patch_3_ind in
  let patch_1_cost, patch_1_time = Patch.get_values patch_1.shape in
  let patch_2_cost, patch_2_time = Patch.get_values patch_2.shape in
  let patch_3_cost, patch_3_time = Patch.get_values patch_3.shape in
  (* determine heuristic score for advancing *)
  let buttons_gained = player_position - ai_position in
  let move_penalty =
    if buttons_gained > 3
    then -2
    else if buttons_gained > 6
    then -4
    else if buttons_gained > 9
    then -5
    else 0
  in
  let opponent_buttons_factor = if player_buttons >= 6 then -2.0 else 1.5 in
  let advance_heuristic =
    float_of_int buttons_gained +. float_of_int move_penalty +. opponent_buttons_factor
  in
  log ("Advance heuristic score: " ^ string_of_float advance_heuristic);
  (* determine heuristic score for placing patch *)
  let afford_patch_1 = ai_buttons - patch_1_cost in
  let afford_patch_2 = ai_buttons - patch_2_cost in
  let afford_patch_3 = ai_buttons - patch_3_cost in
  let patch_1_income = patch_1.income in
  let patch_2_income = patch_2.income in
  let patch_3_income = patch_3.income in
  (* bonus for being behind player after patch placement *)
  let patch_1_extra_turn_bonus =
    if ai_position + patch_1_time < player_position then 3.0 else 0.0
  in
  let patch_2_extra_turn_bonus =
    if ai_position + patch_2_time < player_position then 3.0 else 0.0
  in
  let patch_3_extra_turn_bonus =
    if ai_position + patch_3_time < player_position then 3.0 else 0.0
  in
  (* check all valid board positions for each patch and determine hole penalties *)
  let valid_cells_to_place_patch_1 = test_board_cells patch_1.shape current_state.p2qb in
  let valid_cells_to_place_patch_2 = test_board_cells patch_2.shape current_state.p2qb in
  let valid_cells_to_place_patch_3 = test_board_cells patch_3.shape current_state.p2qb in
  let (p1_best_score : float), p1_best_cell =
    if afford_patch_1 < 0
    then Float.neg_infinity, (1, 1)
    else
      best_score_for_patch
        current_state.p2qb
        patch_1.shape
        patch_1_cost
        patch_1_time
        patch_1_income
        patch_1_extra_turn_bonus
        valid_cells_to_place_patch_1
  in
  let (p2_best_score : float), p2_best_cell =
    if afford_patch_2 < 0
    then Float.neg_infinity, (1, 1)
    else
      best_score_for_patch
        current_state.p2qb
        patch_2.shape
        patch_2_cost
        patch_2_time
        patch_2_income
        patch_2_extra_turn_bonus
        valid_cells_to_place_patch_2
  in
  let (p3_best_score : float), p3_best_cell =
    if afford_patch_3 < 0
    then Float.neg_infinity, (1, 1)
    else
      best_score_for_patch
        current_state.p2qb
        patch_3.shape
        patch_3_cost
        patch_3_time
        patch_3_income
        patch_3_extra_turn_bonus
        valid_cells_to_place_patch_3
  in
  let best_patch, best_cell, best_score =
    if Float.(p1_best_score >= p2_best_score) && Float.(p1_best_score >= p3_best_score)
    then patch_1, p1_best_cell, p1_best_score
    else if
      Float.(p2_best_score >= p1_best_score) && Float.(p2_best_score >= p3_best_score)
    then patch_2, p2_best_cell, p2_best_score
    else patch_3, p3_best_cell, p3_best_score
  in
  log ("Best patch hueristic score: " ^ string_of_float best_score);
  if Float.(best_score >= advance_heuristic)
  then Move.PlacePatch, best_patch.pos_around_board, fst best_cell, snd best_cell
  else Advance, 0, 0, 0
;;
