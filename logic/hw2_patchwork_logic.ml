open! Core

(* Players *)

module Player = struct
  type t = {
    player_num : int;
    player_name : string;
    mutable buttons_owned : int;
    mutable score : int;
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

  type t = {
    shape : patch_shape;
    cost : int;
    pos_around_board : int;
    move_num : int;
    income : int;
  }
  [@@deriving sexp, compare, equal]

  let get_patch_dim p =
    match p with
    | Square -> [ (2, "R"); (1, "D"); (1, "L") ]
    | SquareNub -> [ (3, "D"); (1, "L"); (1, "U") ]
    | SquareHighFive -> [ (2, "D"); (1, "R"); (1, "U"); (1, "R"); (1, "U") ]
    | TCross -> [ (2, "D"); (1, "L"); (1, "SR"); (1, "R"); (1, "SL"); (2, "D") ]
    | S -> [ (2, "L"); (2, "D"); (1, "L") ]
    | LongI -> [ (5, "D") ]
    | LHalfH -> [ (2, "D"); (3, "R"); (1, "U") ]
    | SHalfH -> [ (2, "D"); (2, "R"); (1, "U") ]
    | H -> [ (3, "D"); (1, "SU"); (2, "R"); (1, "U"); (1, "SD"); (1, "D") ]
    | Corner -> [ (2, "R"); (1, "D") ]
    | CornerRev -> [ (2, "D"); (1, "R") ]
    | SLVert -> [ (2, "D"); (1, "R"); (2, "D") ]
    | ShortI -> [ (3, "D") ]
    | I -> [ (4, "D") ]
    | LRev -> [ (3, "D"); (1, "L") ]
    | LongL -> [ (4, "D"); (1, "R") ]
    | L -> [ (3, "D"); (1, "R") ]
    | ChunkyLRev -> [ (4, "D"); (1, "L"); (1, "U") ]
    | SmallI -> [ (2, "D") ]
    | ShortT -> [ (2, "R"); (1, "D"); (1, "SU"); (1, "R") ]
    | StubbyT -> [ (3, "R"); (1, "SL"); (2, "D") ]
    | T -> [ (3, "R"); (1, "SL"); (3, "D") ]
    | Plus -> [ (2, "D"); (1, "L"); (1, "SR"); (1, "R"); (1, "SL"); (1, "D") ]
    | Zig -> [ (2, "D"); (1, "R"); (1, "D") ]
    | ZigZag -> [ (2, "R"); (1, "D"); (1, "R"); (1, "D") ]
    | ZigRev -> [ (2, "D"); (1, "L"); (1, "D") ]
    | ChunkyZig -> [ (3, "D"); (1, "SU"); (1, "L"); (2, "D") ]
    | Cross -> [ (2, "D"); (1, "L"); (1, "SR"); (1, "R"); (1, "SL"); (2, "D") ]
    | INub -> [ (3, "D"); (1, "L"); (1, "SR"); (1, "D") ]
    | WideStubbyT -> [ (2, "R"); (1, "D"); (1, "U"); (1, "R") ]
    | Prong -> [ (2, "D"); (1, "L"); (1, "D"); (1, "SU"); (1, "SR"); (1, "R"); (1, "D") ]
    | Vine -> [ (2, "D"); (1, "L"); (1, "SR"); (1, "D"); (1, "R"); (1, "SL"); (1, "D") ]
    | WidePlus ->
        [
          (2, "R"); (1, "D"); (1, "R"); (1, "SL"); (1, "D"); (1, "L"); (1, "U"); (1, "L");
        ]
    | Empty -> []

  let get_values p =
    match p with
    | Square -> (6, 5)
    | SquareNub -> (2, 2)
    | SquareHighFive -> (8, 6)
    | TCross -> (1, 4)
    | S -> (1, 2)
    | LongI -> (7, 1)
    | LHalfH -> (1, 5)
    | SHalfH -> (1, 2)
    | H -> (2, 3)
    | Corner -> (3, 1)
    | CornerRev -> (1, 3)
    | SLVert -> (2, 3)
    | ShortI -> (2, 2)
    | LRev -> (4, 2)
    | LongL -> (10, 3)
    | L -> (4, 6)
    | ChunkyLRev -> (10, 5)
    | SmallI -> (2, 1)
    | I -> (3, 3)
    | ShortT -> (2, 2)
    | StubbyT -> (5, 5)
    | T -> (7, 2)
    | Plus -> (5, 4)
    | Zig -> (3, 2)
    | ZigZag -> (10, 4)
    | ZigRev -> (7, 6)
    | ChunkyZig -> (4, 2)
    | Cross -> (0, 3)
    | INub -> (3, 4)
    | WideStubbyT -> (7, 4)
    | Prong -> (3, 6)
    | Vine -> (2, 1)
    | WidePlus -> (5, 3)
    | Empty -> (0, 0)

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

  let shapes =
    [
      Square;
      SquareNub;
      SquareHighFive;
      TCross;
      S;
      LongI;
      LHalfH;
      SHalfH;
      H;
      Corner;
      CornerRev;
      SLVert;
      ShortI;
      LRev;
      LongL;
      L;
      ChunkyLRev;
      SmallI;
      I;
      ShortT;
      StubbyT;
      T;
      Plus;
      Zig;
      ZigZag;
      ZigRev;
      ChunkyZig;
      Cross;
      INub;
      WideStubbyT;
      Prong;
      Vine;
      WidePlus;
    ]

  let rec build_patch_set shapes (patches : t list) acc =
    match shapes with
    | [] -> patches
    | hd :: t ->
        let patch_attr = get_values hd in
        let patch_inc = get_income hd in
        let patch =
          {
            shape = hd;
            pos_around_board = acc;
            cost = fst patch_attr;
            move_num = snd patch_attr;
            income = patch_inc;
          }
        in
        let pl_updated = patch :: patches in
        build_patch_set t pl_updated (acc + 1)

  let init_patches = build_patch_set shapes [] 1
end

(* Game Boards *)

module Game_board = struct
  type main_board = { squares : int; special_patch_locs : int list }
  [@@deriving sexp, compare, equal]

  type quilt_board = { squares : int; filled_squares : (int * int) list }
  [@@deriving sexp, compare, equal]

  type t = MainBoard of main_board | QuiltBoard of quilt_board
  [@@deriving sexp, compare, equal]

  exception Out_of_bounds
  exception Patch_does_not_fit_there

  let rec check_patch_squares (f : (int * int) list) qb sr sc dir acc =
    if acc < 1 then (sr, sc)
    else
      let rec check_all_filled (filled_squares : (int * int) list) : bool =
        match filled_squares with
        | [] -> true
        | (r, c) :: tl ->
            if r = sr && c = sc then raise Patch_does_not_fit_there
            else check_all_filled tl
      in

      (* current square not in filled *)
      if check_all_filled f then
        match dir with
        | "D" ->
            if sr < qb.squares then check_patch_squares f qb (sr + 1) sc dir (acc - 1)
            else raise Out_of_bounds
        | "U" ->
            if sr > 1 then check_patch_squares f qb (sr - 1) sc dir (acc - 1)
            else raise Out_of_bounds
        | "L" ->
            if sc > 1 then check_patch_squares f qb sr (sc - 1) dir (acc - 1)
            else raise Out_of_bounds
        | "R" ->
            if sc < qb.squares then check_patch_squares f qb sr (sc + 1) dir (acc - 1)
            else raise Out_of_bounds
        | "SU" -> (sr - 1, sc)
        | "SD" -> (sr + 1, sc)
        | "SL" -> (sr, sc - 1)
        | "SR" -> (sr, sc + 1)
        | _ -> (-1, -1)
      else (-1, -1)

  let rec check_if_patch_fits patchDim board row col =
    match patchDim with
    | [] -> true
    | hd :: tl ->
        let q, d = hd in
        let upd_row, upd_col =
          check_patch_squares board.filled_squares board row col d q
        in
        if upd_row = -1 && upd_col = -1 then false
        else check_if_patch_fits tl board upd_row upd_col

  let place_patch_on_quilt_board board patch r c =
    let dim = Patch.get_patch_dim patch in

    if check_if_patch_fits dim board r c then
      let rec process_dir r c dir filled acc =
        if acc > 0 then
          if r > board.squares || c > board.squares then raise Out_of_bounds
          else
            let nf = (r, c) :: filled in
            match dir with
            | "D" -> process_dir (r + 1) c dir nf (acc - 1)
            | "U" -> process_dir (r - 1) c dir nf (acc - 1)
            | "L" -> process_dir r (c - 1) dir nf (acc - 1)
            | "R" -> process_dir r (c + 1) dir nf (acc - 1)
            | _ -> nf
        else filled
      in

      let rec process_patch patch qb nf r c acc =
        let extract_head l =
          match l with
          | [] -> (-1, -1)
          | hd :: _ -> hd
        in
        match patch with
        | [] ->
            let upd_quilt_board = { board with filled_squares = nf } in
            upd_quilt_board
        | hd :: t ->
            let mv, dir = hd in
            let nr, nc =
              if acc > 1 then
                if String.equal dir "D" then (r + 1, c)
                else if String.equal dir "U" then (r - 1, c)
                else if String.equal dir "L" then (r, c - 1)
                else if String.equal dir "R" then (r, c + 1)
                else if String.equal dir "SD" then (r + 1, c)
                else if String.equal dir "SU" then (r - 1, c)
                else if String.equal dir "SL" then (r, c - 1)
                else if String.equal dir "SR" then (r, c + 1)
                else (r, c)
              else (r, c)
            in

            let new_filled = process_dir nr nc dir nf mv in
            let head = extract_head new_filled in
            let nr = fst head in
            let nc = snd head in
            process_patch t qb new_filled nr nc (acc + 1)
      in
      process_patch dim board board.filled_squares r c 1
    else raise Patch_does_not_fit_there
end

(* Buttons *)

module Button = struct
  type t = { mutable unassigned_cache : int } [@@deriving sexp, compare, equal]

  exception Insufficient_cache
  exception Insufficient_funds

  let give_buttons b (p : Player.t) n =
    if b.unassigned_cache < n then raise Insufficient_cache
    else (
      p.buttons_owned <- p.buttons_owned + n;
      b.unassigned_cache <- b.unassigned_cache - n)

  let take_buttons b (p : Player.t) n =
    if p.buttons_owned < n then raise Insufficient_funds
    else p.buttons_owned <- p.buttons_owned - n;
    b.unassigned_cache <- b.unassigned_cache + n
end

(* Tokens *)

module Token = struct
  type time_token = { position : int; owned_by : Player.t; color : string }
  [@@deriving sexp, compare, equal]

  type neutral_token = { mutable pos : int } [@@deriving sexp, compare, equal]

  type t = TimeToken of time_token | NeutralToken of neutral_token
  [@@deriving sexp, compare, equal]

  let move_token b (t : time_token) (opp : time_token) =
    let opp_pos = opp.position in
    let curr_pos = t.position in
    let distance = abs (opp_pos - curr_pos) in
    Button.give_buttons b t.owned_by (distance + 1);
    let new_token = { t with position = opp_pos + 1 } in
    new_token

  let move_token_after_patch t n =
    let new_token = { t with position = t.position + n } in
    new_token
end

(* Game Pieces *)

module Game_pieces = struct
  type t =
    | TimePiece of Token.t
    | NeutralPiece of Token.t
    | PatchPiece of Patch.t
    | MainBoard of Game_board.main_board
    | QuiltBoard of Game_board.quilt_board
    | Button of Button.t
  [@@deriving sexp, compare, equal]
end

(* Game State *)

module Game_state = struct
  type t = {
    mb : Game_board.main_board;
    p1qb : Game_board.quilt_board;
    p2qb : Game_board.quilt_board;
    bc : Button.t;
    turn : Player.t;
    tk1 : Token.time_token;
    tk2 : Token.time_token;
    neut : Token.neutral_token;
    mutable patches : Patch.t list;
  }
  [@@deriving sexp, compare, equal]

  let update st qb1 qb2 bcache turn tt1 tt2 _neut p =
    {
      st with
      p1qb = qb1;
      p2qb = qb2;
      bc = bcache;
      turn;
      tk1 = tt1;
      tk2 = tt2;
      patches = p;
    }
end

module Move = struct
  type t = Advance | PlacePatch

  let advance_on_board b p1t p2t = Token.move_token b p1t p2t

  exception No_patches_left
  exception Patch_already_taken

  let rec remove_at i pl =
    let empty_patch : Patch.t =
      { shape = Empty; cost = 0; pos_around_board = 0; move_num = 0; income = 0 }
    in
    match pl with
    | [] -> []
    | _ :: tl when i = 0 -> empty_patch :: tl
    | hd :: tl -> hd :: remove_at (i - 1) tl

  let rec take_patch (pl : Patch.t list) choice acc =
    match pl with
    | [] -> raise No_patches_left
    | hd :: tl ->
        if acc = choice then
          match hd.shape with
          | Empty -> raise Patch_already_taken
          | _ -> hd
        else take_patch tl choice (acc + 1)

  let choose_move state mv r c n =
    let player_moving = state.Game_state.turn in
    let player = player_moving.player_num in
    let p1t = state.Game_state.tk1 in
    let p2t = state.Game_state.tk2 in
    let pqb = if player = 1 then state.p1qb else state.p2qb in
    let patches = state.patches in
    let _neut = state.neut in
    match mv with
    | Advance ->
        let new_token =
          advance_on_board state.Game_state.bc
            (if player = 1 then p1t else p2t)
            (if player = 1 then p2t else p1t)
        in
        let next_turn =
          if player = 1 then
            if new_token.position < p2t.position then p1t.owned_by
            else if p2t.position < new_token.position then p2t.owned_by
            else p1t.owned_by
          else if new_token.position < p1t.position then p2t.owned_by
          else if p1t.position < new_token.position then p1t.owned_by
          else p2t.owned_by
        in
        if player = 1 then
          let upd_st =
            Game_state.update state state.p1qb state.p2qb state.bc next_turn new_token p2t
              _neut state.patches
          in
          upd_st
        else
          let upd_st =
            Game_state.update state state.p1qb state.p2qb state.bc next_turn p1t new_token
              _neut state.patches
          in
          upd_st
    | PlacePatch ->
        let p = take_patch patches n 0 in
        let pps = remove_at n patches in
        let qb = Game_board.place_patch_on_quilt_board pqb p.shape r c in
        ignore (Button.take_buttons state.bc player_moving p.cost);
        let new_token =
          Token.move_token_after_patch (if player = 1 then p1t else p2t) p.move_num
        in
        let next_turn =
          if player = 1 then
            if new_token.position < p2t.position then p1t.owned_by
            else if p2t.position < new_token.position then p2t.owned_by
            else p1t.owned_by
          else if new_token.position < p1t.position then p2t.owned_by
          else if p1t.position < new_token.position then p1t.owned_by
          else p2t.owned_by
        in
        _neut.pos <- p.pos_around_board;
        if player = 1 then
          let upd_st =
            Game_state.update state qb state.p2qb state.bc next_turn new_token p2t _neut
              pps
          in
          upd_st
        else
          let upd_st =
            Game_state.update state state.p1qb qb state.bc next_turn p1t new_token _neut
              pps
          in
          upd_st
end

let patches = Patch.init_patches

let p1 =
  {
    Player.player_name = "Jane";
    Player.buttons_owned = 5;
    Player.player_num = 1;
    Player.score = 0;
  }

let p2 =
  {
    Player.player_name = "Bob";
    Player.buttons_owned = 5;
    Player.player_num = 2;
    Player.score = 0;
  }

let mb =
  { Game_board.squares = 64; Game_board.special_patch_locs = [ 20; 32; 46; 57; 62 ] }

let qb1 = { Game_board.squares = 81; Game_board.filled_squares = [] }
let qb2 = { Game_board.squares = 81; Game_board.filled_squares = [] }
let p1Token = { Token.color = "blue"; Token.owned_by = p1; Token.position = 1 }
let p2Token = { Token.color = "red"; Token.owned_by = p2; Token.position = 1 }
let nTok = { Token.pos = 1 }
let buttons = { Button.unassigned_cache = 162 }

let initial_state =
  {
    Game_state.bc = buttons;
    Game_state.mb;
    Game_state.neut = nTok;
    Game_state.p1qb = qb1;
    Game_state.p2qb = qb2;
    Game_state.patches;
    Game_state.tk1 = p1Token;
    Game_state.tk2 = p2Token;
    Game_state.turn = p1;
  }

let _state = initial_state

(*   let make_move = Move.choose_move initial_state PlacePatch 0 0 5
    let _state = make_move *)
