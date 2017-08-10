external findIndex : array 'a => ('a => bool) => int = "" [@@bs.send];

type tile =
  | Walls
  | Person
  | Floor;

let string_of_tile =
  fun
  | Walls => "Walls"
  | Person => "Person"
  | Floor => "Floor";

type direction =
  | Up
  | Left
  | Down
  | Right;

type board = list (list tile);

type state = {board};

let isEqualPos (x, y) (x_, y_) => x == x_ && y == y_;

let getTile (x, y) (board: board) =>
  try {
    let row = List.nth board y;
    Some (List.nth row x)
  } {
  | Failure _ => None
  | Invalid_argument _ => None
  };

let setPosition pos board => {
  let nextTile = getTile pos board;
  List.mapi
    (
      fun y_ =>
        List.mapi (
          fun x_ tile => {
            let isNextFloor =
              switch nextTile {
              | Some Floor => true
              | _ => false
              };
            switch tile {
            | Person when isNextFloor => Floor
            | Floor when isEqualPos pos (x_, y_) => Person
            | _ => tile
            }
          }
        )
    )
    board
};

let flip2 f x y => f y x;

let flippedFindIndex = flip2 findIndex;

let getCurrentPosition board => {
  let index =
    board |> List.flatten |> Array.of_list |>
    flippedFindIndex (
      fun
      | Person => true
      | _ => false
    );
  let dimension = List.length board;
  let y = index / dimension;
  let x = index mod dimension;
  (x, y)
};

let string_of_pos (x, y) => Printf.sprintf "(%d, %d)" x y;

let getNextPosition direction board => {
  let (x, y) = getCurrentPosition board;
  switch direction {
  | Up => (x, y - 1)
  | Down => (x, y + 1)
  | Left => (x - 1, y)
  | Right => (x + 1, y)
  }
};

let move direction board =>
  setPosition (getNextPosition direction board) board;

let renderTile y x tile => {
  let className =
    switch tile {
    | Walls => "tile--wall"
    | Floor => "tile--floor"
    | Person => "tile--person"
    };
  let key = "cell-" ^ string_of_int x ^ "-" ^ string_of_int y;
  <div key className=(key ^ " tile " ^ className) />
};

let toTileRow y => List.mapi (renderTile y);

let component = ReasonReact.statefulComponent "App";

let keyCodeToDirection event =>
  switch (ReactEventRe.Keyboard.keyCode event) {
  | 37 => Some Left
  | 38 => Some Up
  | 39 => Some Right
  | 40 => Some Down
  | _ => None
  };

let doMove event {ReasonReact.state: state} =>
  switch (keyCodeToDirection event) {
  | Some direction =>
    ReactEventRe.Keyboard.preventDefault event;
    ReasonReact.Update {board: move direction state.board}
  | _ => ReasonReact.Update state
  };

let board = [
  [Floor, Floor, Floor, Floor, Walls, Floor, Floor, Floor, Floor],
  [Walls, Walls, Walls, Floor, Walls, Floor, Walls, Walls, Floor],
  [Floor, Floor, Floor, Floor, Walls, Floor, Walls, Floor, Floor],
  [Floor, Walls, Walls, Walls, Walls, Floor, Walls, Floor, Walls],
  [Floor, Floor, Floor, Floor, Floor, Floor, Walls, Floor, Floor],
  [Walls, Walls, Walls, Walls, Walls, Walls, Walls, Walls, Floor],
  [Floor, Floor, Floor, Floor, Walls, Floor, Floor, Floor, Floor],
  [Floor, Walls, Walls, Floor, Floor, Floor, Walls, Walls, Walls],
  [Floor, Walls, Walls, Walls, Walls, Walls, Walls, Walls, Walls]
];

let board = setPosition (0, 0) board;

let state = {board: board};

let make _children => {
  ...component,
  initialState: fun () => state,
  render: fun {state, update} => {
    let tiles = state.board |> List.mapi toTileRow |> List.flatten;
    <div className="App">
      <div className="frame" tabIndex=0 onKeyDown=(update doMove)>
        (ReasonReact.arrayToElement (Array.of_list tiles))
      </div>
    </div>
  }
};
