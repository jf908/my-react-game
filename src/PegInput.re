type state = {
    guess: list(Game.symbol)
};

type action =
    | Add(Game.symbol)
    | Clear
    | Submit;

let component = ReasonReact.reducerComponent("PegInput");

let rec makeList = (x, value) =>
    switch(x) {
    | 0 => []
    | n => [value, ...makeList(n-1, value)]
    };

let make = (~pegTypes, ~guessLength, ~onSubmit, _children) => {
    ...component,

    initialState: () => {guess: []},

    reducer: (action, state) =>
        switch(action) {
        | Add(_) when List.length(state.guess) >= guessLength => ReasonReact.NoUpdate
        | Add(s) => ReasonReact.Update({guess: [s, ...state.guess] })
        | Clear => ReasonReact.Update({guess: []})
        | Submit => switch(List.length(state.guess)) {
            | 4 => ReasonReact.UpdateWithSideEffects(
                {guess: []},
                (_self => onSubmit(state.guess))
            )
            | _ => ReasonReact.NoUpdate
            }
        },

    render: ({state, send}) => {
        let currentPeg = state.guess
            |> List.map(s => Some(s))
            |> List.append(makeList(guessLength - List.length(state.guess), None))
            |> List.rev
            |> List.map(s => switch(s) {
                | None => <div className="peg-empty"></div>
                | Some(c) => <div className={"peg " ++ String.make(1, c)}></div>
            });
        let pegTypesEl = List.map(c =>
            <div className={"peg " ++ String.make(1, c)} onClick=(_ => send(Add(c)))></div>
        , pegTypes);
        <div className="input">
            <div className="peg-input-container">
                <button className="clear" onClick=(_ => send(Clear))>{ReasonReact.string("Clear")}</button>
                <div className="peg-input">
                    {ReasonReact.array(Array.of_list(currentPeg))}
                </div>
                <button className="submit" onClick=(_ => send(Submit))>{ReasonReact.string("Submit")}</button>
            </div>
            <div className="pegs">
                {ReasonReact.array(Array.of_list(pegTypesEl))}
            </div>
        </div>
    }
};