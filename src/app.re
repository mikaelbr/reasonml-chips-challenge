type color =
  | Red
  | Blue
  | Green;

let string_of_color =
  fun
  | Red => "Red"
  | Blue => "Blue"
  | Green => "Green";

type tile =
  | Walls
  | Person
  | Key color
  | Door color
  | Floor;

let string_of_tile =
  fun
  | Walls => "Walls"
  | Person => "Person"
  | Floor => "Floor"
  | Key c => "Key: " ^ string_of_color c
  | Door c => "Door: " ^ string_of_color c;

type direction =
  | Up
  | Left
  | Down
  | Right;

type board = list (list tile);

type state = {
  board,
  keys: list color
};

let isEqualPos (x, y) (x_, y_) => x == x_ && y == y_;

let getTile (x, y) (board: board) =>
  try {
    let row = List.nth board y;
    Some (List.nth row x)
  } {
  | Failure _ => None
  | Invalid_argument _ => None
  };

let setPosition pos {board, keys} :state => {
  let nextTile = getTile pos board;
  let (board, newKeys) =
    board |> Array.of_list |>
    Js.Array.reducei
      (
        fun (acc, keys_) row y_ => {
          let (newRow, newKeys) =
            row |> Array.of_list |>
            Js.Array.reducei
              (
                fun (newRow, prevKeys) tile x_ => {
                  let isNextPositionLegal =
                    switch nextTile {
                    | Some (Door c) when List.exists (fun a => a == c) keys =>
                      true
                    | Some Floor
                    | Some (Key _) => true
                    | _ => false
                    };
                  let (newTile, key) =
                    switch tile {
                    | Person when isNextPositionLegal => (Floor, prevKeys)
                    | Floor when isEqualPos pos (x_, y_) => (Person, prevKeys)
                    | Key c when isEqualPos pos (x_, y_) => (
                        Person,
                        prevKeys @ [Some (Key c)]
                      )
                    | Door c
                        when isNextPositionLegal && isEqualPos pos (x_, y_) => (
                        Person,
                        prevKeys
                      )
                    | _ => (tile, prevKeys)
                    };
                  (newRow @ [newTile], key)
                }
              )
              ([], []);
          (acc @ [newRow], keys_ @ newKeys)
        }
      )
      ([], []);
  let newKeys =
    newKeys |>
    List.filter (
      fun
      | None => false
      | Some (Key _) => true
      | _ => false
    ) |>
    List.map (
      fun
      | Some (Key c) => c
      | _ => failwith "Unreachable code"
    );
  {board, keys: keys @ newKeys}
};

let getCurrentPosition board => {
  let index =
    board |> List.flatten |> Array.of_list |>
    Js.Array.findIndex (
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

let move direction state =>
  setPosition (getNextPosition direction state.board) state;

let renderTile y x tile => {
  let className =
    switch tile {
    | Walls => "tile--wall"
    | Floor => "tile--floor"
    | Person => "tile--person"
    | Key c => "tile--key" ^ string_of_color c
    | Door c => "tile--door" ^ string_of_color c
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
    let state = move direction state;
    let foo = state.keys |> List.map string_of_color |> String.concat ", ";
    Js.log foo;
    ReasonReact.Update state
  | _ => ReasonReact.NoUpdate
  };

let board = [
  [Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Floor, Key Red, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Key Red, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor],
  [Floor, Door Red, Floor, Floor, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor]
];

let state = {board, keys: []};

let state = setPosition (0, 0) state;

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
