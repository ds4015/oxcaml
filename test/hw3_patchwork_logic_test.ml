open! Core
open Hw2_patchwork_logic

(* Player *)
(* test player creation and mutable field alteration *)

let%expect_test "create_player" =
  let p : Player.t =
    { player_num = 1; player_name = "Dallas"; buttons_owned = 5; score = 0 }
  in
  print_s [%sexp (p : Player.t)];
  [%expect {| ((player_num 1) (player_name Dallas) (buttons_owned 5) (score 0)) |}];
  p.buttons_owned <- 9;
  p.score <- 15;
  print_s [%sexp (p : Player.t)];
  [%expect {| ((player_num 1) (player_name Dallas) (buttons_owned 9) (score 15)) |}]

(* Patch *)
(* test the patch list generator - 33 patches total *)

let%expect_test "init_patches" =
  let pl : Patch.t list = Patch.init_patches in
  let rec iter_patches patches =
    match patches with
    | [] -> ()
    | hd :: tl ->
        print_s [%sexp (hd : Patch.t)];
        [%expect
          {|
          (* CR expect_test: Test ran multiple times with different test outputs *)
          ============================= Output 1 / 33 =============================
          ((shape WidePlus) (cost 5) (pos_around_board 33) (move_num 3) (income 1))

          ============================= Output 2 / 33 =============================
          ((shape Vine) (cost 2) (pos_around_board 32) (move_num 1) (income 0))

          ============================= Output 3 / 33 =============================
          ((shape Prong) (cost 3) (pos_around_board 31) (move_num 6) (income 2))

          ============================= Output 4 / 33 =============================
          ((shape WideStubbyT) (cost 7) (pos_around_board 30) (move_num 4) (income 2))

          ============================= Output 5 / 33 =============================
          ((shape INub) (cost 3) (pos_around_board 29) (move_num 4) (income 1))

          ============================= Output 6 / 33 =============================
          ((shape Cross) (cost 0) (pos_around_board 28) (move_num 3) (income 1))

          ============================= Output 7 / 33 =============================
          ((shape ChunkyZig) (cost 4) (pos_around_board 27) (move_num 2) (income 0))

          ============================= Output 8 / 33 =============================
          ((shape ZigRev) (cost 7) (pos_around_board 26) (move_num 6) (income 3))

          ============================= Output 9 / 33 =============================
          ((shape ZigZag) (cost 10) (pos_around_board 25) (move_num 4) (income 3))

          ============================ Output 10 / 33 =============================
          ((shape Zig) (cost 3) (pos_around_board 24) (move_num 2) (income 1))

          ============================ Output 11 / 33 =============================
          ((shape Plus) (cost 5) (pos_around_board 23) (move_num 4) (income 2))

          ============================ Output 12 / 33 =============================
          ((shape T) (cost 7) (pos_around_board 22) (move_num 2) (income 2))

          ============================ Output 13 / 33 =============================
          ((shape StubbyT) (cost 5) (pos_around_board 21) (move_num 5) (income 2))

          ============================ Output 14 / 33 =============================
          ((shape ShortT) (cost 2) (pos_around_board 20) (move_num 2) (income 0))

          ============================ Output 15 / 33 =============================
          ((shape I) (cost 3) (pos_around_board 19) (move_num 3) (income 1))

          ============================ Output 16 / 33 =============================
          ((shape SmallI) (cost 2) (pos_around_board 18) (move_num 1) (income 0))

          ============================ Output 17 / 33 =============================
          ((shape ChunkyLRev) (cost 10) (pos_around_board 17) (move_num 5) (income 3))

          ============================ Output 18 / 33 =============================
          ((shape L) (cost 4) (pos_around_board 16) (move_num 6) (income 2))

          ============================ Output 19 / 33 =============================
          ((shape LongL) (cost 10) (pos_around_board 15) (move_num 3) (income 2))

          ============================ Output 20 / 33 =============================
          ((shape LRev) (cost 4) (pos_around_board 14) (move_num 2) (income 1))

          ============================ Output 21 / 33 =============================
          ((shape ShortI) (cost 2) (pos_around_board 13) (move_num 2) (income 0))

          ============================ Output 22 / 33 =============================
          ((shape SLVert) (cost 2) (pos_around_board 12) (move_num 3) (income 1))

          ============================ Output 23 / 33 =============================
          ((shape CornerRev) (cost 1) (pos_around_board 11) (move_num 3) (income 0))

          ============================ Output 24 / 33 =============================
          ((shape Corner) (cost 3) (pos_around_board 10) (move_num 1) (income 0))

          ============================ Output 25 / 33 =============================
          ((shape H) (cost 2) (pos_around_board 9) (move_num 3) (income 0))

          ============================ Output 26 / 33 =============================
          ((shape SHalfH) (cost 1) (pos_around_board 8) (move_num 2) (income 0))

          ============================ Output 27 / 33 =============================
          ((shape LHalfH) (cost 1) (pos_around_board 7) (move_num 5) (income 1))

          ============================ Output 28 / 33 =============================
          ((shape LongI) (cost 7) (pos_around_board 6) (move_num 1) (income 1))

          ============================ Output 29 / 33 =============================
          ((shape S) (cost 1) (pos_around_board 5) (move_num 2) (income 0))

          ============================ Output 30 / 33 =============================
          ((shape TCross) (cost 1) (pos_around_board 4) (move_num 4) (income 1))

          ============================ Output 31 / 33 =============================
          ((shape SquareHighFive) (cost 8) (pos_around_board 3) (move_num 6)
           (income 3))

          ============================ Output 32 / 33 =============================
          ((shape SquareNub) (cost 2) (pos_around_board 2) (move_num 2) (income 0))

          ============================ Output 33 / 33 =============================
          ((shape Square) (cost 6) (pos_around_board 1) (move_num 5) (income 2))
          |}];
        iter_patches tl;
        ()
  in
  iter_patches pl

(* let%test_unit "patch_dimension" =
  [%test_eq: (int * string) list] (Patch.get_patch_dim L) [ (3, "D"); (1, "R") ];
  [%test_eq: (int * string) list] (Patch.get_patch_dim TCross)
  [ (2, "D"); (1, "L"); (1, "SR"); (1, "R"); (1, "SL"); (2, "D") ] *)

let pretty_print_qb (quilt_board : Game_board.quilt_board) =
  let check_if_filled r c filled =
    let rec parse_filled f =
      match f with
      | [] -> false
      | hd :: tl ->
          let rf, cf = hd in
          if rf = r && cf = c then true else parse_filled tl
    in
    parse_filled filled
  in
  for x = 1 to quilt_board.squares do
    for y = 1 to quilt_board.squares do
      if check_if_filled x y quilt_board.filled_squares then print_string "[X]"
      else print_string "[ ]"
    done;
    print_string "\n"
  done

let%expect_test "print_quilt_board" =
  let qb : Game_board.quilt_board = { squares = 9; filled_squares = [ (5, 4) ] } in
  pretty_print_qb qb;
  [%expect
    {|
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][X][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    |}]

(* Helper tests - ignore (not exposed via interface) *)
(* let%expect_test "place_patch" =
  let patch : Patch.patch_shape = L in
  let qb : Game_board.quilt_board = { squares = 8; filled_squares = [ (5, 4) ] } in
  let qb = Game_board.place_patch_on_quilt_board qb patch 1 2 in
  pretty_print_qb qb;
  [%expect
    {|
    [ ][X][ ][ ][ ][ ][ ][ ]
    [ ][X][ ][ ][ ][ ][ ][ ]
    [ ][X][X][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][X][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ]
    |}];

  let patch : Patch.patch_shape = ZigZag in
  let qb : Game_board.quilt_board =
    { squares = 8; filled_squares = [ (1, 2); (2, 2); (3, 2); (3, 3); (5, 4) ] }
  in
  let qb = Game_board.place_patch_on_quilt_board qb patch 6 4 in
  pretty_print_qb qb;
  [%expect
    {|
    [ ][X][ ][ ][ ][ ][ ][ ]
    [ ][X][ ][ ][ ][ ][ ][ ]
    [ ][X][X][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][X][ ][ ][ ][ ]
    [ ][ ][ ][X][X][ ][ ][ ]
    [ ][ ][ ][ ][X][X][ ][ ]
    [ ][ ][ ][ ][ ][X][ ][ ]
    |}];

  let patch : Patch.patch_shape = Plus in
  let qb : Game_board.quilt_board = { squares = 8; filled_squares = [] } in
  let qb = Game_board.place_patch_on_quilt_board qb patch 4 5 in
  pretty_print_qb qb;
  [%expect
    {|
    [ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][X][ ][ ][ ]
    [ ][ ][ ][X][X][X][ ][ ]
    [ ][ ][ ][ ][X][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ]
    |}]

  let%expect_test "move_token" =
  let p1 : Player.t = { player_num = 1; player_name = "Me"; buttons_owned = 5; score = 0 } in
  let t1 : Token.time_token = { position = 0; owned_by = p1; color = "Red" } in
  let p2 : Player.t = { player_num = 2; player_name = "AI"; buttons_owned = 5; score = 0 } in
  let t2 : Token.time_token = { position = 5; owned_by = p2; color = "Blue" } in
  let b : Button.t = { unassigned_cache = 150 } in
  Token.move_token b t1 t2;
  print_s [%sexp (t1 : Token.time_token)];
  [%expect {| ((position 6) (owned_by ((player_num 1) (player_name Me) (buttons_owned 11) (score 0))) (color Red)) |}] *)

(* STATE TESTS *)

(* Test Move 1: Advance Forward on Board *)

let%expect_test "make_move_adv_forward" =
  let mb : Game_board.main_board = { squares = 76; special_patch_locs = [] } in
  let p1qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let p2qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let cache : Button.t = { unassigned_cache = 162 } in
  let p1 : Player.t =
    { player_num = 1; player_name = "Me"; buttons_owned = 5; score = 0 }
  in
  let p2 : Player.t =
    { player_num = 2; player_name = "AI"; buttons_owned = 5; score = 0 }
  in
  let tk1 : Token.time_token = { position = 1; owned_by = p1; color = "Red" } in
  let tk2 : Token.time_token = { position = 1; owned_by = p2; color = "Blue" } in
  let nt : Token.neutral_token = { pos = 5 } in
  let patches = Patch.init_patches in
  let initial_state : Game_state.t =
    { mb; p1qb; p2qb; bc = cache; turn = p1; tk1; tk2; neut = nt; patches }
  in
  let updated_state : Game_state.t = Move.choose_move initial_state Advance 0 0 0 in
  print_s [%sexp (updated_state.tk1 : Token.time_token)];
  [%expect
    {|
    ((position 2)
     (owned_by ((player_num 1) (player_name Me) (buttons_owned 6) (score 0)))
     (color Red)) |}];
  print_s [%sexp (updated_state.turn : Player.t)];
  [%expect {| ((player_num 2) (player_name AI) (buttons_owned 5) (score 0)) |}]

(* Test Move 2: Purchase Patch and Place on Quilt Board, then Advance *)

let%expect_test "make_move_place_patch" =
  let mb : Game_board.main_board = { squares = 76; special_patch_locs = [] } in
  let p1qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let p2qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let cache : Button.t = { unassigned_cache = 162 } in
  let p1 : Player.t =
    { player_num = 1; player_name = "Me"; buttons_owned = 5; score = 0 }
  in
  let p2 : Player.t =
    { player_num = 2; player_name = "AI"; buttons_owned = 5; score = 0 }
  in
  let tk1 : Token.time_token = { position = 1; owned_by = p1; color = "Red" } in
  let tk2 : Token.time_token = { position = 1; owned_by = p2; color = "Blue" } in
  let nt : Token.neutral_token = { pos = 5 } in
  let patches = Patch.init_patches in

  let initial_state : Game_state.t =
    { mb; p1qb; p2qb; bc = cache; turn = p1; tk1; tk2; neut = nt; patches }
  in
  let updated_state : Game_state.t = Move.choose_move initial_state PlacePatch 4 5 19 in
  print_s [%sexp (updated_state.tk1 : Token.time_token)];
  [%expect
    {|
    ((position 3)
     (owned_by ((player_num 1) (player_name Me) (buttons_owned 1) (score 0)))
     (color Red)) |}];
  pretty_print_qb updated_state.p1qb;
  [%expect
    {|
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][X][ ][ ][ ][ ]
    [ ][ ][ ][ ][X][ ][ ][ ][ ]
    [ ][ ][ ][X][X][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    [ ][ ][ ][ ][ ][ ][ ][ ][ ]
    |}]

(* Test Place Patch Move - Error: No more patches available *)

let%expect_test "place_patch_error_no_more_patches" =
  let mb : Game_board.main_board = { squares = 76; special_patch_locs = [] } in
  let p1qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let p2qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let cache : Button.t = { unassigned_cache = 162 } in
  let p1 : Player.t =
    { player_num = 1; player_name = "Me"; buttons_owned = 5; score = 0 }
  in
  let p2 : Player.t =
    { player_num = 2; player_name = "AI"; buttons_owned = 5; score = 0 }
  in
  let tk1 : Token.time_token = { position = 1; owned_by = p1; color = "Red" } in
  let tk2 : Token.time_token = { position = 1; owned_by = p2; color = "Blue" } in
  let nt : Token.neutral_token = { pos = 5 } in
  let patches = [] in
  let initial_state : Game_state.t =
    { mb; p1qb; p2qb; bc = cache; turn = p1; tk1; tk2; neut = nt; patches }
  in
  let message =
    try
      let _upd_state = Move.choose_move initial_state PlacePatch 4 5 19 in
      "Success."
    with
    | Move.No_patches_left -> "Error: There are no more patches to purchase."
    | exn -> "Unexpected: " ^ Exn.to_string exn
  in
  print_string message;
  [%expect {| Error: There are no more patches to purchase. |}]

(* Test Place Patch Move - Error: Insufficient funds *)

let%expect_test "place_patch_error_insufficient_funds" =
  let mb : Game_board.main_board = { squares = 76; special_patch_locs = [] } in
  let p1qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let p2qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let cache : Button.t = { unassigned_cache = 162 } in
  let p1 : Player.t =
    { player_num = 1; player_name = "Me"; buttons_owned = 5; score = 0 }
  in
  let p2 : Player.t =
    { player_num = 2; player_name = "AI"; buttons_owned = 5; score = 0 }
  in
  let tk1 : Token.time_token = { position = 1; owned_by = p1; color = "Red" } in
  let tk2 : Token.time_token = { position = 1; owned_by = p2; color = "Blue" } in
  let nt : Token.neutral_token = { pos = 5 } in
  let patches = Patch.init_patches in
  let initial_state : Game_state.t =
    { mb; p1qb; p2qb; bc = cache; turn = p1; tk1; tk2; neut = nt; patches }
  in
  let message =
    try
      let _upd_state = Move.choose_move initial_state PlacePatch 4 5 27 in
      "Success."
    with
    | Button.Insufficient_funds ->
        "Error: You do not have enough buttons to purchase that patch."
    | exn -> "Unexpected: " ^ Exn.to_string exn
  in
  print_string message;
  [%expect {| Error: You do not have enough buttons to purchase that patch. |}]

(* Test Place Patch Move - Error: Chosen Position on Quilt Board Out of Bounds *)

let%expect_test "place_patch_error_out_of_bounds" =
  let mb : Game_board.main_board = { squares = 76; special_patch_locs = [] } in
  let p1qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let p2qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let cache : Button.t = { unassigned_cache = 162 } in
  let p1 : Player.t =
    { player_num = 1; player_name = "Me"; buttons_owned = 5; score = 0 }
  in
  let p2 : Player.t =
    { player_num = 2; player_name = "AI"; buttons_owned = 5; score = 0 }
  in
  let tk1 : Token.time_token = { position = 1; owned_by = p1; color = "Red" } in
  let tk2 : Token.time_token = { position = 1; owned_by = p2; color = "Blue" } in
  let nt : Token.neutral_token = { pos = 5 } in
  let patches = Patch.init_patches in
  let initial_state : Game_state.t =
    { mb; p1qb; p2qb; bc = cache; turn = p1; tk1; tk2; neut = nt; patches }
  in
  let message =
    try
      let _upd_state = Move.choose_move initial_state PlacePatch 4 1 19 in
      pretty_print_qb _upd_state.p1qb;
      "Success."
    with
    | Game_board.Out_of_bounds -> "Error: That location on quilt board is out of bounds."
    | exn -> "Unexpected: " ^ Exn.to_string exn
  in
  print_string message;
  [%expect {| Error: That location on quilt board is out of bounds. |}]

(* Test Place Patch Move - Error: Patch Choice Already Removed from Pool *)

let%expect_test "place_patch_error_patch_already_taken" =
  let mb : Game_board.main_board = { squares = 76; special_patch_locs = [] } in
  let p1qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let p2qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let cache : Button.t = { unassigned_cache = 162 } in
  let p1 : Player.t =
    { player_num = 1; player_name = "Me"; buttons_owned = 15; score = 0 }
  in
  let p2 : Player.t =
    { player_num = 2; player_name = "AI"; buttons_owned = 5; score = 0 }
  in
  let tk1 : Token.time_token = { position = 1; owned_by = p1; color = "Red" } in
  let tk2 : Token.time_token = { position = 1; owned_by = p2; color = "Blue" } in
  let nt : Token.neutral_token = { pos = 5 } in
  let patches = Patch.init_patches in
  let initial_state : Game_state.t =
    { mb; p1qb; p2qb; bc = cache; turn = p1; tk1; tk2; neut = nt; patches }
  in
  let _upd_st = Move.choose_move initial_state PlacePatch 4 8 19 in
  let message =
    try
      let _upd_state = Move.choose_move _upd_st PlacePatch 3 3 19 in
      "Success."
    with
    | Move.Patch_already_taken ->
        "Error: That patch was already taken.  Please choose another."
    | exn -> "Unexpected: " ^ Exn.to_string exn
  in
  print_string message;
  [%expect {| Error: That patch was already taken.  Please choose another. |}]

(* Test Place Patch Move Series(x3) - Success -> Success -> Error: Patch Won't Fit in Chosen Location *)

let%expect_test "place_patch_error_patch_does_not_fit" =
  let mb : Game_board.main_board = { squares = 76; special_patch_locs = [] } in
  let p1qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let p2qb : Game_board.quilt_board = { squares = 9; filled_squares = [] } in
  let cache : Button.t = { unassigned_cache = 162 } in
  let p1 : Player.t =
    { player_num = 1; player_name = "Me"; buttons_owned = 17; score = 0 }
  in
  let p2 : Player.t =
    { player_num = 2; player_name = "AI"; buttons_owned = 6; score = 0 }
  in
  let tk1 : Token.time_token = { position = 1; owned_by = p1; color = "Red" } in
  let tk2 : Token.time_token = { position = 1; owned_by = p2; color = "Blue" } in
  let nt : Token.neutral_token = { pos = 5 } in
  let patches = Patch.init_patches in
  let initial_state : Game_state.t =
    { mb; p1qb; p2qb; bc = cache; turn = p1; tk1; tk2; neut = nt; patches }
  in
  let st1 = Move.choose_move initial_state PlacePatch 4 8 6 in
  print_s [%sexp (p1 : Player.t)];
  [%expect {| ((player_num 1) (player_name Me) (buttons_owned 13) (score 0)) |}];
  let msg1, st2 =
    try
      let st2 = Move.choose_move st1 PlacePatch 4 8 19 in
      ("Success.", st2)
    with
    | Game_board.Patch_does_not_fit_there ->
        ( "Error: That patch will not fit in that location.  Choose another spot on your \
           quilt board.",
          st1 )
  in
  print_string msg1;
  [%expect {| Success. |}];
  let msg2 =
    try
      let st3 = Move.choose_move st2 PlacePatch 4 8 4 in
      pretty_print_qb st3.p1qb;
      "Success."
    with
    | Game_board.Patch_does_not_fit_there ->
        "Error: That patch will not fit in that location.  Choose another spot on your \
         quilt board."
    | exn -> "Unexpected: " ^ Exn.to_string exn
  in
  print_string msg2;
  [%expect
    {| Error: That patch will not fit in that location.  Choose another spot on your quilt board. |}]
