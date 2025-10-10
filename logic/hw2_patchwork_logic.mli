(* Player *)

module Player : sig
  type t = {
    player_num : int;
    player_name : string;
    mutable buttons_owned : int;
    mutable score : int
  } [@@deriving sexp, compare, equal]
end


(* Patch *)

module Patch : sig
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

  (* initialize set of patches for new game *)
  val init_patches : t list
end

(* Game Boards *)

module Game_board : sig
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
end


(* Buttons *)

module Button : sig
  type t = {
    mutable unassigned_cache : int
  } [@@deriving sexp, compare, equal]

end


(* Tokens *)

module Token : sig
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
end

(* Game Pieces *)

module Game_pieces : sig
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

module Game_state : sig
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
end

(* Move *)

module Move : sig
  type t =
    | Advance
    | PlacePatch

  (* Choose to advance or take/place patch, returns updated game state *)
  val choose_move : Game_state.t -> t-> int -> int -> int -> Game_state.t
end
