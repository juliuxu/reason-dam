type player =
  | W
  | B;

type piece =
  | None
  | P(player)
  | Q(player);

module Styles = {
  open Css;

  let squareSize = 8 * 8;

  let row = style([display(flexBox)]);

  let square = even =>
    style([
      display(flexBox),
      alignItems(center),
      justifyContent(center),
      background(even ? lightgoldenrodyellow : darkred),
      width(px(squareSize)),
      height(px(squareSize)),
      cursor(`pointer),
    ]);

  let queen = color => style([]);
  let peasant = color =>
    style([
      width(pct(60.)),
      height(pct(50.)),
      borderRadius(pct(50.)),
      border(px(3), solid, color),
      backgroundColor(color),
    ]);
  let piece = piece =>
    switch (piece) {
    | None => style([])
    | P(W) => peasant(white)
    | Q(W) => queen(white)
    | P(B) => peasant(black)
    | Q(B) => queen(black)
    };
};

module Piece = {
  [@react.component]
  let make = (~piece) => {
    <div className={Styles.piece(piece)} />;
  };
};

module Square = {
  [@react.component]
  let make = (~even, ~children) => {
    <div className={Styles.square(even)}> children </div>;
  };
};

type state = {rows: list(list(piece))};

let state = {
  rows: [
    [None, P(W), None, P(W), None, P(W), None, P(W)],
    [P(W), None, P(W), None, P(W), None, P(W), None],
    [None, P(W), None, P(W), None, P(W), None, P(W)],
    [None, None, None, None, None, None, None, None],
    [None, None, None, None, None, None, None, None],
    [P(B), None, P(B), None, P(B), None, P(B), None],
    [None, P(B), None, P(B), None, P(B), None, P(B)],
    [P(B), None, P(B), None, P(B), None, P(B), None],
  ],
};

[@react.component]
let make = () => {
  <div>
    {state.rows
     |> List.mapi((rowi, row) =>
          <div className=Styles.row key={string_of_int(rowi)}>
            {row
             |> List.mapi((columni, piece) =>
                  <Square
                    even={(columni + rowi) mod 2 === 0}
                    key={string_of_int(columni)}>
                    <Piece piece />
                  </Square>
                )
             |> Array.of_list
             |> React.array}
          </div>
        )
     |> Array.of_list
     |> React.array}
  </div>;
};