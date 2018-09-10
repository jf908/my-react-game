let component = ReasonReact.statelessComponent("SingleGame");

let rec makeList = (x, value) =>
    switch(x) {
    | 0 => []
    | n => [value, ...makeList(n-1, value)]
    };

let make = (~game as (player, actualCode, guesses), _children) => {
    ...component,

    render: (_self) => {
        let pegElList = c => {
            let s = String.make(1, c);
            <div className={"peg " ++ s}>
                {ReasonReact.string(s)}
            </div>
        };
        
        let scoreElList = ((blackMarkers, whiteMarkers)) => {
            makeList(blackMarkers, <div className="black marker"></div>)
            @
            makeList(whiteMarkers, <div className="white marker"></div>)
            @
            makeList(Game.pegs - blackMarkers - whiteMarkers, <div className="marker"></div>);
        };

        let turns = guesses
            |> List.mapi((i,guess) => {
                <div className="guess">
                    <span className="turn">{ReasonReact.string(string_of_int(List.length(guesses) - i))}</span>
                    <div className="peg-turn">
                        {ReasonReact.array(Array.of_list(List.map(pegElList, List.rev(guess))))}
                        {ReasonReact.array(Array.of_list(scoreElList(Game.score(actualCode, guess))))}
                    </div>
                </div>
            });
        <div>
            <div className="message">
                {ReasonReact.string((player == Game.Human ? "You are" : "The AI is") ++ " the codemaker!")}
            </div>
            {ReasonReact.array(Array.of_list(turns))}
        </div>
    }
};