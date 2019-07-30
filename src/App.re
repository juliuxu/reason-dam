module Styles = {
  open Css;
  let root =
    style([display(flexBox), flexDirection(column), alignItems(center)]);
  let newGame = style([fontSize(px(28))]);
};

[@react.component]
let make = () => {
  <div className=Styles.root>
    <h1> {{j|👹 Chaos Dam 👹|j} |> React.string} </h1>
    <State>
      ...{(state: GameState.state, dispatch) =>
        <React.Fragment>
          <Board state dispatch />
          <h2>
            {(
               switch (state.active, GameState.getWinner(state.board)) {
               | (player, None) =>
                 GameState.playerToString(player) ++ " to move"
               | (_, Some(winner)) =>
                 {j|🎉 |j}
                 ++ GameState.playerToString(winner)
                 ++ {j| Wins! 🎉|j}
               }
             )
             |> React.string}
          </h2>
          <div>
            {switch (GameState.getWinner(state.board)) {
             | None => <span />
             | _ =>
               <button
                 onClick={_ => dispatch(GameState.Reset)}
                 className=Styles.newGame>
                 {{j| 👺New Game|j} |> React.string}
               </button>
             }}
          </div>
        </React.Fragment>
      }
    </State>
  </div>;
};