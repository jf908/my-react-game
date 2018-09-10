let pegs : int = 4;

type symbol = char;

let symbols : list(symbol) = ['a','b','c','d','e','f'];

type code = list(symbol);

type score = (int, int);

type player = Human | Computer;

let codemaker : player = Human;

let firstGuess : code = ['a','a','b','b'];

let correctGuess = ((blackMarkers, _)) => blackMarkers == pegs;

let validateCode = code => List.length(code) == pegs;

let rec codeGen = fun
    | 1 => List.map(s => [s], symbols)
    | x => List.concat(List.map(s => List.map(code => [s, ...code], codeGen(x-1)), symbols));

let codes : list(code) = codeGen(pegs);

let results : list(score) = {
    let results = ref([]);
    for(blackMarkers in 0 to pegs) {
        for(whiteMarkers in 0 to pegs - blackMarkers) {
            if(!(blackMarkers == pegs-1 && whiteMarkers == 1)) {
                results := [(blackMarkers, whiteMarkers), ...results^];
            }
        }
    }
    results^;
};

let scoreCorPos = (code, guess) => List.length @@ List.filter(((c,g)) => c==g) @@ List.combine(code, guess);

let delete = (x, xs) => {
    let (ys,zs) = List.partition(((!=)(x)), xs)
    ys @ List.tl(zs)
};


let rec scoreAnyPos = (code, guess) =>
    switch(code) {
    | [] => 0
    | [s, ...ss] when List.mem(s, guess) => 1 + (scoreAnyPos(ss, delete(s, guess)))
    | [_, ...ss] => scoreAnyPos(ss, guess)
    };

let score = (code, guess) => {
    let corPos = scoreCorPos(code, guess);
    let anyPos = scoreAnyPos(code, guess);
    (corPos, anyPos - corPos);
};

let maximum = List.fold_left(max, 0);

let maxRemaining = (codes : list(code), guess : code) => {
    let scores = List.map(c => score(c, guess), codes);
    let eliminations = x => List.length(List.filter((==)(x), scores));
    maximum(List.map(eliminations, results));
};

let nextGuess = (available : list(code)) => {
    switch(available) {
    | [code, ...[]] => code
    | s => {
        let resultScores = List.map(r => (r, maxRemaining(s, r)), s @ codes);
        let smaller = ((r1, m1), (r2, m2)) => m2 < m1 ? (r2, m2) : (r1, m1);
        fst @@ List.fold_left(smaller, List.hd(resultScores), List.tl(resultScores));
    }
    };
};

let eliminate = (lastScore, guess, codes) => List.filter(c => lastScore == score(c, guess), codes);