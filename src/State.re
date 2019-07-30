[@react.component]
let make = (~children) => {
  let (state, dispatch) =
    React.useReducer(GameState.reducer, GameState.make());
  children(state, dispatch);
};