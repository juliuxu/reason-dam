type player =
  | W
  | B;

let playerToString = player =>
  switch (player) {
  | W => "White"
  | B => "Black"
  };

type piece =
  | N0ne
  | P(player)
  | Q(player);

type position = (int, int);

type actions =
  | Reset
  | Select(position)
  | Move(position, position);
type board = list(list(piece));
type state = {
  active: player,
  selected: option(position),
  board,
};

let makeBoard = () => [
  [N0ne, P(B), N0ne, P(B), N0ne, P(B), N0ne, P(B)],
  [P(B), N0ne, P(B), N0ne, P(B), N0ne, P(B), N0ne],
  [N0ne, P(B), N0ne, P(B), N0ne, P(B), N0ne, P(B)],
  [N0ne, N0ne, N0ne, N0ne, N0ne, N0ne, N0ne, N0ne],
  [N0ne, N0ne, N0ne, N0ne, N0ne, N0ne, N0ne, N0ne],
  [P(W), N0ne, P(W), N0ne, P(W), N0ne, P(W), N0ne],
  [N0ne, P(W), N0ne, P(W), N0ne, P(W), N0ne, P(W)],
  [P(W), N0ne, P(W), N0ne, P(W), N0ne, P(W), N0ne],
];
let make = () => {active: W, selected: None, board: makeBoard()};

module Util = {
  let get = (board: board, (rowi, columni): position) =>
    List.nth(List.nth(board, rowi), columni);

  let isValidMove = (board, from: position, to_: position) => true;

  // https://github.com/puemos/reasonml-astar-maze/blob/master/src/modules/utils/Distance.re
  let euclideanDistance = ((x1, y1), (x2, y2)) =>
    abs(x2 - x1) + abs(y2 - y1);

  // https://stackoverflow.com/questions/17692922/check-is-a-point-x-y-is-between-two-points-drawn-on-a-straight-line
  let isBetween = (from, to_, position) => {
    euclideanDistance(from, position)
    + euclideanDistance(to_, position) == euclideanDistance(from, to_);
  };

  let isBetween = ((x1, y1), (x2, y2), (x3, y3)) =>
    (x1 - x3) * (y1 - y3) == (x3 - x2) * (y3 - y2);
};

let maybePromoteToQueen = (position, piece) =>
  switch (position, piece) {
  | ((0, _), P(W)) => Q(W)
  | ((7, _), P(B)) => Q(B)
  | _ => piece
  };

let flattenBoard = (board: board) =>
  board
  |> List.mapi((x, row) =>
       row |> List.mapi((y, piece) => ((x, y), piece))
     )
  |> List.flatten;

let getWinner = (board: board) => {
  let (whitePieces, blackPieces) =
    board
    |> List.map(x => x |> List.map(y => y))
    |> List.flatten
    |> List.map(piece =>
         switch (piece) {
         | N0ne => None
         | P(W) => Some(W)
         | Q(W) => Some(W)
         | P(B) => Some(B)
         | Q(B) => Some(B)
         }
       )
    |> List.filter(x => x != None)
    |> List.partition(x => x == Some(W));

  switch (List.length(whitePieces), List.length(blackPieces)) {
  | (0, _) => Some(B)
  | (_, 0) => Some(W)
  | _ => None
  };
};

let move = (board: board, from: position, to_: position) => {
  let fromPiece = Util.get(board, from);
  let (x1, y1) = from;
  let (x2, y2) = to_;
  Js.log("Move");
  Js.log(from);
  Js.log(to_);
  board
  |> List.mapi((x3, row) =>
       row
       |> List.mapi((y3, piece) =>
            switch (x3, y3) {
            | _ when to_ != (x3, y3) && Util.isBetween(from, to_, (x3, y3)) =>
              N0ne
            | _ when (x1, y1) == (x3, y3) => N0ne
            | _ when (x2, y2) == (x3, y3) =>
              maybePromoteToQueen(to_, fromPiece)
            | _ => piece
            }
          )
     );
};
/*
  (0, 0)
  (4, 4)
  (2, 2)
 */

let reducer = (state, action) => {
  switch (action) {
  | Reset => make()
  | Select(position) => {...state, selected: Some(position)}
  | Move(from, to_) when Util.isValidMove(state.board, from, to_) => {
      selected: None,
      active:
        switch (state.active) {
        | W => B
        | B => W
        },
      board: move(state.board, from, to_),
    }
  | Move(_, _) => state
  };
};