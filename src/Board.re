type player =
  | W
  | B;

type piece =
  | None
  | P(player)
  | Q(player);

module Styles = {
  open Css;

  module Theme = {
    let squareSize = 8 * 8;
    let evenSquareColor = lightgoldenrodyellow;
    let oddSquareColor = darkred;

    let playerWColor = white;
    let playerBColor = hex("2A2A2A");

    let playerToColor = player =>
      switch (player) {
      | W => playerWColor
      | B => playerBColor
      };

    let playerToColorInverted = player =>
      switch (player) {
      | W => playerToColor(B)
      | B => playerToColor(W)
      };
  };
  let row = style([display(flexBox)]);

  let square = even =>
    style([
      display(flexBox),
      alignItems(center),
      justifyContent(center),
      background(even ? Theme.evenSquareColor : Theme.oddSquareColor),
      width(px(Theme.squareSize)),
      height(px(Theme.squareSize)),
      cursor(`pointer),
    ]);

  let peasant = player =>
    style([
      width(pct(60.)),
      height(pct(50.)),
      borderRadius(pct(50.)),
      border(px(3), solid, Theme.playerToColor(player)),
      backgroundColor(Theme.playerToColor(player)),
      boxShadow(~x=px(0), ~y=px(5), ~blur=px(2), rgba(50, 50, 50, 0.75)),
    ]);

  let queen = player =>
    Css.merge([
      peasant(player),
      style([
        before([
          display(flexBox),
          justifyContent(center),
          alignItems(center),
          unsafe("content", {j|"♕"|j}),
          fontSize(px(20)),
          fontWeight(bold),
          marginTop(px(2)),
          color(Theme.playerToColorInverted(player)),
        ]),
      ]),
    ]);

  let piece = piece =>
    switch (piece) {
    | None => style([])
    | P(W) => peasant(W)
    | Q(W) => queen(W)
    | P(B) => peasant(B)
    | Q(B) => queen(B)
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

let initState = {
  rows: [
    [None, Q(W), None, Q(W), None, Q(W), None, Q(W)],
    [Q(W), None, Q(W), None, Q(W), None, Q(W), None],
    [None, Q(W), None, Q(W), None, Q(W), None, Q(W)],
    [None, None, None, None, None, None, None, None],
    [None, None, None, None, None, None, None, None],
    [Q(B), None, Q(B), None, Q(B), None, Q(B), None],
    [None, Q(B), None, Q(B), None, Q(B), None, Q(B)],
    [Q(B), None, Q(B), None, Q(B), None, Q(B), None],
  ],
};

[@react.component]
let make = () => {
  <div>
    {initState.rows
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