open Game;

type gameState = (player, code, list(code));

type state = {
    games: list(gameState)
};

type action = NextTurn(code);

let component = ReasonReact.reducerComponent("App");

Random.self_init();

let randomSymbol = () => List.nth(symbols, Random.int(List.length(symbols)));
let rec randomCode = fun
    | 0 => []
    | x => [randomSymbol(), ...randomCode(x-1)];

let newComTurn = (Computer, randomCode(pegs), []);
let newHumTurn = (Human, [], []);

let comPlay = (actualCode) => {
    let rec play = (turns, actualCode, guess, remaining) => {
        if(actualCode == guess) {
            [guess, ...turns]
        } else {
            let newRemaining = eliminate(score(actualCode, guess), guess, remaining);
            play([guess, ...turns], actualCode, nextGuess(newRemaining), newRemaining);
        }
    };
    (Human, actualCode, play([], actualCode, firstGuess, codes));
};

let make = (_children) => {
    ...component,

    initialState: () => {games: [newHumTurn]},

    reducer: (action, state) => {
        let [(codemaker, actualCode, guesses), ...games] = state.games;
        switch(action) {
        | NextTurn(next) => switch(codemaker) {
            | Human => ReasonReact.Update({ games: [newComTurn, comPlay(next), ...games]})
            | Computer => switch(actualCode == next) {
                | true => ReasonReact.Update({
                    games: [newHumTurn, (Computer, actualCode, [next,...guesses]), ...games]
                })
                | false => ReasonReact.Update({
                    games: [(Computer, actualCode, [next,...guesses]), ...games]
                })
            }
        }
        }
    },

    render: ({state, send}) => {
        <div className="container">
            <div className="board">
                {ReasonReact.array(Array.of_list(List.map(game => {
                    <SingleGame game=game />
                }, state.games)))}
            </div>
            <PegInput pegTypes=Game.symbols guessLength=Game.pegs onSubmit=(guess => send(NextTurn(guess))) />
        </div>
    }
};