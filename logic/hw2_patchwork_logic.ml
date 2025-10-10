(* Players *)

module Player = struct
  type t = {
    player_num : int;
    player_name : string;
    mutable buttons_owned : int;
    mutable score : int
  } [@@deriving sexp, compare, equal]
end


(* Patches *)

module Patch = struct
  type patch_shape =
    | L
    | I
    | Z
    | T
    | H
    | Square
    | Smallest
    | SmallI
    | SFatTail
    | Plus
    | LargePlus
    | LargeL
    | HalfH
    | HalfHLong
    | ChunkyL
    | BackwardL
    | ZigZag

  type t = {
    shape : patch_shape;
    cost: int;
    pos_around_board: int;
    move_num : int
  } [@@deriving sexp, compare, equal]


  let get_patch_dim p =
    match p with
        L -> [3,"D"; 1,"R"]
      | I -> [5, "D"]
      | Z -> [2, "R"; 2 , "D"; 1, "R"]
      | SFatTail -> [2, "L"; 1, "D"; 1, "L"; 1, "D"; 1, "R"]
      | Square -> [2, "R"; 1, "D"; 1, "L"]
      | Smallest -> [2, "D"; 1, "R"]
      | SmallI -> [3, "D"]
      | T -> [2, "D"; 1, "L"; 1, "SR"; 1, "R"; 1, "L"; 2, "D"]
      | Plus -> [2, "D"; 1, "L"; 1, "SR"; 1, "R"; 1, "SL"; 1, "D"]
      | LargePlus -> [2, "D"; 1, "L"; 2, "D"; 2, "R"; 2, "U"; 1, "SL"; 1, "D"; 1, "SD"; 1, "D"]
      | LargeL -> [4, "D"; 1, "L"]
      | HalfH -> [2, "D"; 1, "SU"; 2, "R"; 1, "D"]
      | HalfHLong -> [1, "D"; 1, "SU"; 3, "R"; 1, "D"]
      | H -> [1, "D"; 1, "SU"; 2, "R"; 1, "U"; 1, "SD"; 1, "D"]
      | ChunkyL -> [3, "D"; 1, "R"; 1, "D"; 1, "L"; 1, "U"]
      | BackwardL -> [4, "D"; 1, "L"]
      | ZigZag -> [2, "R"; 1, "D"; 1, "R"; 1, "D"]

  let get_values p =
    match p with
      L -> (4, 2)
    | I -> (7, 1)
    | Z -> (1, 2)
    | Square -> (6, 5)
    | Smallest -> (3, 1)
    | SmallI -> (2, 2)
    | SFatTail -> (9, 8)
    | T -> (0, 3)
    | Plus -> (5, 4)
    | LargePlus -> (5, 3)
    | LargeL -> (10, 3)
    | HalfH -> (1, 2)
    | HalfHLong -> (1, 5)
    | H -> (2, 3)
    | ChunkyL -> (10, 5)
    | BackwardL -> (4, 2)
    | ZigZag -> (10, 4)

  let shapes = [L; I; Z; Square; Smallest; T; Plus; LargePlus; H; HalfH; HalfHLong; SmallI; SFatTail; ChunkyL; BackwardL; ZigZag]


  let rec build_patch_set shapes (patches : t list) acc =
    let rec add_patches n p patches =
      if n < 2 then
        let new_patch_list = p::patches in
        add_patches (n+1) p new_patch_list
      else patches
    in
    match shapes with
       [] -> patches
     | hd::t ->
      let patch_attr = get_values hd in
      let patch = { shape = hd; pos_around_board = acc; cost = fst patch_attr; move_num = snd patch_attr} in
      let interim_patches = add_patches 0 patch patches in
      build_patch_set t interim_patches (acc + 1)

  let init_patches = build_patch_set shapes [] 1
end


(* Game Boards *)

module Game_board = struct
  type main_board = {
    squares : int;
    special_patch_locs : int list
  }

  type quilt_board = {
    squares : int;
    filled_squares : (int * int) list
  }

  type t =
    | MainBoard of main_board
    | QuiltBoard of quilt_board
    [@@deriving sexp, compare, equal]

exception Out_of_bounds

let rec check_patch_squares f qb sr sc dir acc =
  if acc < 1 then (sr, sc) else

    let rec check_all_filled f =
      match f with
        [] -> true
       | hd::tl -> let (r,c) = hd in
                   if (r == sr && c == sc) then false
                   else check_all_filled tl
     in

     if check_all_filled f then
       match dir with
          "D" -> if sr <= qb.squares then check_patch_squares f qb (sr+1) sc dir (acc-1) else raise Out_of_bounds
        | "U" -> if sr > 0 then check_patch_squares f qb (sr-1) sc dir (acc-1) else raise Out_of_bounds
        | "L" -> if sc > 0 then check_patch_squares f qb sr (sc-1) dir (acc-1) else raise Out_of_bounds
        | "R" -> if sc <= qb.squares then check_patch_squares f qb sr (sc+1) dir (acc-1) else raise Out_of_bounds
        | _ -> (-1,-1)
     else (-1,-1)


let rec check_if_patch_fits patchDim board row col =
  match patchDim with
    [] -> true
    | hd::tl ->
      let (q,d) = hd in
      let (upd_row, upd_col) = check_patch_squares board.filled_squares board row col d q in
      if upd_row == -1 && upd_col == -1 then false else
        check_if_patch_fits tl board upd_row upd_col

exception Patch_does_not_fit_there

let place_patch_on_quilt_board board patch r c =
  let dim = Patch.get_patch_dim patch in

    if check_if_patch_fits dim board r c then

      let rec process_dir r c dir filled acc =
          let nf = (r,c)::filled in
          if acc > 0 then
            match dir with
                "D" -> process_dir (r+1) c dir nf (acc-1)
              | "U" -> process_dir (r-1) c dir nf (acc-1)
              | "L" -> process_dir r (c-1) dir nf (acc-1)
              | "R" -> process_dir r (c+1) dir nf (acc-1)
              | _ -> nf
          else nf
      in

      let rec process_patch patch qb nf =
        match patch with
          [] -> let upd_quilt_board = { board with filled_squares = nf } in
                upd_quilt_board
          | hd::t -> let (mv,dir) = hd in
            let new_filled = process_dir r c dir board.filled_squares mv in
            process_patch t qb new_filled
      in
      process_patch dim board board.filled_squares
      else raise Patch_does_not_fit_there
end

(* Buttons *)

module Button = struct
  type t = {
    mutable unassigned_cache : int
  } [@@deriving sexp, compare, equal]

  exception Insufficient_cache
  exception Insufficient_funds

  let give_buttons b (p : Player.t) n =
    if (b.unassigned_cache < n) then raise Insufficient_cache
    else
      begin
        p.buttons_owned <- (p.buttons_owned + n);
        b.unassigned_cache <- b.unassigned_cache - n
      end

  let take_buttons b (p : Player.t) n =
    if (p.buttons_owned < n) then raise Insufficient_funds
    else
      p.buttons_owned = p.buttons_owned - n &&
      b.unassigned_cache = b.unassigned_cache + n
end

(* Tokens *)

module Token = struct
  type time_token = {
    mutable position : int;
    owned_by : Player.t;
    color : string
  }

  type neutral_token = {
    mutable pos: int
  }

  type t =
    | TimeToken of time_token
    | NeutralToken of neutral_token
    [@@deriving sexp, compare, equal]

  let move_token b t opp =
    let opp_pos = opp.position in
    let curr_pos = t.position in
    let distance = abs(opp_pos - curr_pos) in
    Button.give_buttons b t.owned_by (distance + 1);
    let t' = {t with position = curr_pos + distance + 1} in
    Printf.printf "Moved player %d to position %d" t.owned_by.player_num t'.position

  let move_token_after_patch t n =
    t.position <- t.position + n
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
      mb: Game_board.main_board;
      p1qb : Game_board.quilt_board;
      p2qb: Game_board.quilt_board;
      bc : Button.t;
      turn: Player.t;
      tk1: Token.time_token;
      tk2: Token.time_token;
      neut: Token.neutral_token;
      patches: Patch.t list;
  } [@@deriving sexp, compare, equal]

  let update st qb1 qb2 bcache turn tt1 tt2 neut p =
    { st with p1qb = qb1; p2qb = qb2; bc = bcache; turn = turn; tk1 = tt1; tk2 = tt2; patches = p}
end


module Move = struct
  type t =
    | Advance
    | PlacePatch

  let advance_on_board b p1t p2t = Token.move_token b p1t p2t

  exception No_patches_left
  let rec take_patch (pl : Patch.t list) choice acc =
    match pl with
      [] -> raise No_patches_left
    | hd::tl -> if acc = choice then hd
      else take_patch tl choice (acc + 1)


  let choose_move state mv r c n =
    let player_moving = state.Game_state.turn in
    let player = player_moving.player_num in
    let p1t = if player = 1 then state.Game_state.tk1 else state.Game_state.tk2 in
    let p2t = if player = 2 then state.Game_state.tk2 else state.Game_state.tk1 in
    let pqb = if player = 1 then state.p1qb else state.p2qb in
    let patches = state.patches in
    let neut = state.neut in
    match mv with
        Advance -> advance_on_board state.Game_state.bc p1t p2t;
          if player = 1 then
          let upd_st = Game_state.update state state.p1qb state.p2qb state.bc state.tk2.owned_by p1t p2t neut state.patches in
          upd_st else
          let upd_st = Game_state.update state state.p1qb state.p2qb state.bc state.tk1.owned_by p1t p2t neut state.patches in
          upd_st
      | PlacePatch -> let p = take_patch patches n 0 in
        let qb = Game_board.place_patch_on_quilt_board pqb p.shape r c in
        ignore (Token.move_token_after_patch p1t p.move_num);
        ignore (Button.take_buttons state.Game_state.bc player_moving p.cost);
        neut.pos <- p.pos_around_board;
        if player = 1 then
          let upd_st = Game_state.update state qb state.p2qb state.bc state.tk2.owned_by p1t p2t neut state.patches in
          upd_st
        else
          let upd_st = Game_state.update state state.p1qb qb state.bc state.tk1.owned_by p1t p2t neut state.patches in
          upd_st
end

  let patches = Patch.init_patches

  let p1 = { Player.player_name = "Jane"; Player.buttons_owned = 5; Player.player_num = 1; Player.score = 0}
  let p2 = { Player.player_name = "Bob"; Player.buttons_owned = 5; Player.player_num = 2; Player.score = 0}

  let mb = { Game_board.squares = 64; Game_board.special_patch_locs = [20; 32; 46; 57; 62]}
  let qb1 = { Game_board.squares = 81; Game_board.filled_squares = []}
  let qb2 = { Game_board.squares = 81; Game_board.filled_squares = []}
  let p1Token = { Token.color = "blue"; Token.owned_by = p1; Token.position = 1}
  let p2Token = { Token.color = "red"; Token.owned_by = p2; Token.position = 1}
  let nTok = { Token.pos = 1}
  let buttons = { Button.unassigned_cache = 162 }

  let initial_state = {
    Game_state.bc = buttons;
    Game_state.mb = mb;
    Game_state.neut = nTok;
    Game_state.p1qb = qb1;
    Game_state.p2qb = qb2;
    Game_state.patches = patches;
    Game_state.tk1 = p1Token;
    Game_state.tk2 = p2Token;
    Game_state.turn = p1
  }

  let state = initial_state

  let make_move = Move.choose_move initial_state PlacePatch 0 0 5
  let state = make_move

