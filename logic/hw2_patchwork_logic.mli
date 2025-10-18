open! Core

(* Player *)

module Player : sig
  type t = {
    player_num : int;
    player_name : string;
    mutable buttons_owned : int;
    mutable score : int;
  }
  [@@deriving sexp, compare, equal]
end

(* Patch *)

module Patch : sig
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
end

(* Game Boards *)

module Game_board : sig
  type main_board = { squares : int; special_patch_locs : int list }
  [@@deriving sexp, compare, equal]

  type quilt_board = { squares : int; filled_squares : (int * int) list }
  [@@deriving sexp, compare, equal]

  type t = MainBoard of main_board | QuiltBoard of quilt_board
  [@@deriving sexp, compare, equal]

  exception Out_of_bounds
  exception Patch_does_not_fit_there
end

(* Buttons *)

module Button : sig
  type t = { mutable unassigned_cache : int } [@@deriving sexp, compare, equal]

  exception Insufficient_funds
  exception Insufficient_cache
end

(* Tokens *)

module Token : sig
  type time_token = { position : int; owned_by : Player.t; color : string }
  [@@deriving sexp, compare, equal]

  type neutral_token = { pos : int } [@@deriving sexp, compare, equal]

  type t = TimeToken of time_token | NeutralToken of neutral_token
  [@@deriving sexp, compare, equal]
end

(* Game Pieces *)

module Game_pieces : sig
  type t = {
    player1 : Player.t;
    player2 : Player.t;
    time_piece1 : Token.time_token;
    time_piece2 : Token.time_token;
    neutral_piece : Token.neutral_token;
    patch_pieces : Patch.t list;
    patches_remaining : int list;
    main_board : Game_board.main_board;
    quilt_board1 : Game_board.quilt_board;
    quilt_board2 : Game_board.quilt_board;
    buttons : Button.t;
  }
  [@@deriving sexp, compare, equal]

  val setup_game : string -> string -> string -> string -> t
end

(* Game State *)

module Game_state : sig
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
    patches_remaining : int list;
  }
  [@@deriving sexp, compare, equal]
end

(* Move *)

module Move : sig
  type t = Advance | PlacePatch

  exception No_patches_left
  exception Patch_already_taken

  (* Choose to advance or take/place patch, returns updated game state *)
  val choose_move :
    (unit -> int) -> (string -> unit) -> Game_state.t -> t -> int -> int -> Game_state.t
end

val _init : unit -> Game_state.t
