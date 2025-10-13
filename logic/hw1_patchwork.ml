type patch_shapes =
    L
  | T
  | I
  | Square
  | Z
  | Plus

type quilt = {
    emptyCells : int;
    filledCells : (int * int) list
}

type game_board =
    TimeBoard of int
  | QuiltBoard of quilt   

type tokens =
    TimeToken of int
  | NeutralToken of int

type game_state = {
    time_board : game_board;
    p1quilt : game_board;
    p2quilt : game_board;
    p1token : tokens;
    p2token : tokens;
    neutral_token : tokens;
    p1buttons : int;
    p2buttons : int
}

let getPatchDims patch =
 match patch with
   L -> 1, 3
 | T -> 3, 2
 | I -> 0, 5
 | Square -> 2, 2
 | Z -> 2, 3
 | Plus -> 3, 3

let getPlayerCurr p state = 
  if p = 1 then state.p1buttons
  else state.p2buttons

let p1quilt = { emptyCells = 64; filledCells = []}
let p2quilt = { emptyCells = 64; filledCells = []}

let timeBoard = TimeBoard 81
let p1QuiltBoard = QuiltBoard p1quilt
let p2QuiltBoard = QuiltBoard p2quilt
let p1TimeToken = TimeToken 0
let p2TimeToken = TimeToken 0
let neutralToken = NeutralToken 0

let initialState = {
  time_board = timeBoard; p1quilt = p1QuiltBoard;
  p2quilt = p2QuiltBoard; p1token = p1TimeToken; p2token = p2TimeToken;
  neutral_token = neutralToken; p1buttons = 5; p2buttons = 5
}

let moveToken tok n = 
  match tok with
    TimeToken t -> TimeToken (t + n)
  | NeutralToken p -> NeutralToken (p + n)

let advance tok1 tok2 =
  match tok1, tok2 with
   TimeToken t1, t2 -> moveToken tok1 (t2 - t1)
  | NeutralToken n, _ -> moveToken tok1 1;;


let p1curr = getPlayerCurr 1 initialState;;
let p1TimeToken = moveToken p1TimeToken 4

let p1QuiltBoard = QuiltBoard { emptyCells = 61; filledCells = [(6,3); (6,4); (7,3)]}

let interimState = { 
  time_board = timeBoard; p1quilt = p1QuiltBoard; p2quilt = p2QuiltBoard;
  p1token = p1TimeToken; p2token = p2TimeToken; neutral_token = neutralToken;
  p1buttons = 2; p2buttons = 5}

let p2QuiltBoard = QuiltBoard { emptyCells = 35; filledCells = [(1,3); (1,4);
 (2,2); (2,3); (2,4); (3,1); (3,2); (3,4); (3,5); (3,7); 
 (4,1); (4,2); (4,4); (4,5); (4,6); (4,7); 
 (5,5); (5,7); (6,3); (6,4); (6,2); (6,7); (6,8); (7,3); (7,7); (7,8);
 (8,3); (8,7); (8,8)]}

let finalState =  { 
  time_board = timeBoard; p1quilt = p1QuiltBoard; p2quilt = p2QuiltBoard;
  p1token = p1TimeToken; p2token = p2TimeToken; neutral_token = neutralToken;
  p1buttons = 12; p2buttons = 45}
