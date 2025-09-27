(* Players *)

module Player = struct
  type player = {
    player_num : int;
    player_name : string;
    mutable buttons_owned : int;
    mutable score : int
  }
end


(* Patches *)

module Patch = struct
  type patch_shape =
    | L
    | I
    | Z
    | Square
    | Smallest
    | T
    | Plus
    | LargePlus
    | H
    | ChunkyL
    | BackwardL
    | ZigZag

  type patch = {
    shape : patch_shape;
    cost: int;
    move_num : int
  }


  let get_patch_dim p =
    match p with
        L -> [3,"D"; 1,"R"]
      | I -> [5, "D"]
      | Z -> [2, "R"; 2 , "D"; 1, "R"]
      | Square -> [2, "R"; 1, "D"; 1, "L"]
      | Smallest -> [2, "D"; 1, "R"]
      | T -> [2, "D"; 1, "L"; 1, "SR"; 1, "R"; 1, "L"; 2, "D"]
      | Plus -> [2, "D"; 1, "L"; 1, "SR"; 1, "R"; 1, "SL"; 1, "D"]
      | LargePlus -> [2, "D"; 1, "L"; 2, "D"; 2, "R"; 2, "U"; 1, "SL"; 1, "D"; 1, "SD"; 1, "D"]
      | H -> [1, "D"; 1, "SU"; 2, "R"; 1, "U"; 1, "SD"; 1, "D"]
      | ChunkyL -> [3, "D"; 1, "R"; 1, "D"; 1, "L"; 1, "U"]
      | BackwardL -> [4, "D"; 1, "L"]
      | ZigZag -> [2, "R"; 1, "D"; 1, "R"; 1, "D"]

  let shapes = [L; I; Z; Square; Smallest; T; Plus; LargePlus; H; ChunkyL; BackwardL; ZigZag]


  let rec build_patch_set shapes (patches : patch list) =
    match shapes with
       [] -> patches
  | hd::t -> let rec add_patches n patches =
      if (n = 0) then patches else
        let patch = { shape = hd; cost = 4; move_num = 6} in
        add_patches (n-1) (patch::patches)
      in
      build_patch_set t patches

  let init_patches = build_patch_set shapes []
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

  type game_board =
    | MainBoard of main_board
    | QuiltBoard of quilt_board


let rec check_patch_square f sr sc =
  match f with
    [] -> false
    | hd::t ->
      let (row, col) = hd in
      if (row = sr && col = sc) then true
      else check_patch_square t sr sc
end

(*
let rec check_patch_fit b dim loc =
  let (r,c) = loc in
  match dim with
      [] -> true
    | hd::t -> let (mv, dir) = hd in
        match dir with
            "D" -> if (check_patch_square b.filled_squares r c+mv) = false then false
              else check_patch_fit b t loc
          | "U" -> if (check_patch_square b.filled_squares r c-mv) = false then false
            else check_patch_fit b t loc
          | "L" -> if not (check_patch_square b.filled_squares r-mv c) then false
            else check_patch_fit b t loc
          | "R" -> if not (check_patch_square b.filled_squares r+mv c) then false
            else check_patch_fit b t loc
          | "SD" | "SU" | "SL" | "SR" -> check_patch_fit b t loc
          | _ -> check_patch_fit b t loc

let rec add_patch_to_filled loc dim filled =
  let (r,c) = loc in
  match dim with
     [] -> filled
| hd::t -> let (mv, dir) = hd in
  match dir with
    "D" -> (r+mv, c) :: filled; let upd_loc = (r+mv, c) in add_patch_to_filled upd_loc t filled
  | "U" -> (r-mv, c) :: filled; let upd_loc = (r-mv, c) in add_patch_to_filled upd_loc t filled
  | "L" -> (r, c-mv) :: filled; let upd_loc = (r, c-mv) in add_patch_to_filled upd_loc t filled
  | "R" -> (r, c+mv) :: filled; let upd_loc = (r, c+mv) in add_patch_to_filled upd_loc t filled
  | _ -> add_patch_to_filled loc t filled
    filled


let place_patch bcache board p loc tk =
  if check_patch_fit board (get_patch_dim p.shape) loc then
  take_buttons bcache tk.owned_by p.cost &&
  b.filled_squares = add_patch_to_filled loc (get_patch_dim p.shape) board.filled_squares &&
  move_token_after_patch tk p.move_num
  else false

  *)

(* Buttons *)

module Button = struct
  type button = {
    mutable unassigned_cache : int
  }

  exception Insufficient_cache
  exception Insufficient_funds

  let give_buttons b (p : Player.player) n =
    if (b.unassigned_cache < n) then raise Insufficient_cache
    else
      begin
        p.buttons_owned <- (p.buttons_owned + n);
        b.unassigned_cache <- b.unassigned_cache - n
      end

  let take_buttons b (p : Player.player) n =
    if (p.buttons_owned < n) then raise Insufficient_funds
    else
      p.buttons_owned = p.buttons_owned - n &&
      b.unassigned_cache = b.unassigned_cache + n
end

(* Tokens *)

module Token = struct
  type time_token = {
    mutable position : int;
    owned_by : Player.player;
    color : string
  }

  type neutral_token = {
    pos: int
  }


  type token =
    | TimeToken of time_token
    | NeutralToken of neutral_token

  let move_token b t opp =
    let opp_pos = opp.position in
    let curr_pos = t.position in
    let distance = abs(opp_pos - curr_pos) in
    Button.give_buttons b t.owned_by (distance + 1);
    let t' = {t with position = curr_pos + distance + 1} in
    Printf.printf "Moved player %d to position %d" t.owned_by.player_num t'.position

  let move_token_after_patch t n =
    t.position = t.position + n
end

(* Game Pieces *)

module Game_pieces = struct
  type game_pieces =
    | TimePiece of Token.token
    | NeutralPiece of Token.token
    | PatchPiece of Patch.patch
    | MainBoard of Game_board.game_board
    | QuiltBoard of Game_board.game_board
    | Button of Button.button
end


(* Game State *)

module Game_state = struct
  type game_state = {
      mb: Game_board.main_board;
      p1qb : Game_board.quilt_board;
      p2qb: Game_board.quilt_board;
      bc : Button.button;
      turn: Player.player;
      tk1: Token.time_token;
      tk2: Token.time_token;
      neut: Token.neutral_token;
      patches: Patch.patch list;
  }

  let update st qb1 qb2 bcache turn tt1 tt2 neut p pl =
    { st with p1qb = qb1; p2qb = qb2; bc = bcache; turn = pl; tk1 = tt1; tk2 = tt2; patches = p}
end


module Make_move = struct
  let choose_move b p1t p2t =
    Token.move_token b p1t p2t
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

  let make_move = Make_move.choose_move initial_state.bc initial_state.tk1 initial_state.tk2
