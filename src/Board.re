module Styles = {
  open Css;
  open GameState;

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

  let square = (even, selectable) =>
    style([
      display(flexBox),
      alignItems(center),
      justifyContent(center),
      background(even ? Theme.evenSquareColor : Theme.oddSquareColor),
      width(px(Theme.squareSize)),
      height(px(Theme.squareSize)),
      cursor(selectable ? `pointer : `default),
    ]);

  let peasant = (player, selected) =>
    style([
      width(pct(60.)),
      height(pct(50.)),
      borderRadius(pct(50.)),
      border(px(3), solid, selected ? gold : Theme.playerToColor(player)),
      backgroundColor(Theme.playerToColor(player)),
      boxShadow(~x=px(0), ~y=px(5), ~blur=px(2), rgba(50, 50, 50, 0.75)),
    ]);

  let queen = (player, selected) =>
    Css.merge([
      peasant(player, selected),
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

  let piece = (piece, selected) =>
    switch (piece) {
    | N0ne => style([])
    | P(W) => peasant(W, selected)
    | Q(W) => queen(W, selected)
    | P(B) => peasant(B, selected)
    | Q(B) => queen(B, selected)
    };

  let root = style([border(px(8), solid, black)]);
};

module Piece = {
  [@react.component]
  let make = (~piece, ~selected) => {
    <div className={Styles.piece(piece, selected)} />;
  };
};

module Square = {
  [@react.component]
  let make = (~even, ~onClick, ~children) => {
    <div onClick className={Styles.square(even, true)}> children </div>;
  };
};

[@react.component]
let make = (~state: GameState.state, ~dispatch) => {
  Js.log("render");
  let onSquareClick = (rowi, columni, _) => {
    let position: GameState.position = (rowi, columni);
    // TODO: Move logic out to reducer?
    // Have Click actions instead?
    Js.log("onSquareClick");
    switch (state.selected) {
    | Some(selectedPosition) =>
      switch (GameState.Util.get(state.board, position)) {
      | GameState.N0ne when (rowi + columni) mod 2 !== 0 =>
        dispatch(GameState.Move(selectedPosition, position))
      | P(player)
      | Q(player) when player == state.active =>
        dispatch(GameState.Select(position))
      | _ => ()
      }
    | None =>
      switch (GameState.Util.get(state.board, position)) {
      | P(player)
      | Q(player) when player == state.active =>
        dispatch(GameState.Select(position))
      | GameState.N0ne => ()
      | _ => ()
      }
    };
  };
  <div className=Styles.root>
    {state.board
     |> List.mapi((rowi, row) =>
          <div className=Styles.row key={string_of_int(rowi)}>
            {row
             |> List.mapi((columni, piece) =>
                  <Square
                    onClick={onSquareClick(rowi, columni)}
                    even={(columni + rowi) mod 2 === 0}
                    key={string_of_int(columni)}>
                    <Piece
                      selected={state.selected == Some((rowi, columni))}
                      piece
                    />
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