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

let string_of_opt_tile =
  fun
  | Some x => string_of_tile x
  | None => "Nothing";

let printKeyList prefix keys =>
  Js.log (
    prefix ^ " :: " ^ (keys |> List.map string_of_color |> String.concat ", ")
  );

let isEqualPos (x, y) (x_, y_) => x == x_ && y == y_;

let fold_lefti fn init arr => arr |> Array.of_list |> Js.Array.reducei fn init;

let getTile (x, y) (board: board) =>
  try {
    let row = List.nth board y;
    Some (List.nth row x)
  } {
  | Failure _ => None
  | Invalid_argument _ => None
  };

let hasKeyOfColor c keys => List.exists ((==) c) keys;

let rec removeOneFromKeys c =>
  fun
  | [h, ...t] when h == c => t
  | [h, ...t] => [h, ...removeOneFromKeys c t]
  | [] => [];

let isNextPositionLegal keys =>
  fun
  | Some (Door c) when hasKeyOfColor c keys => true
  | Some Floor
  | Some (Key _) => true
  | _ => false;

let getNextTile inCurrentPos prevKeys =>
  fun
  | Person => (Floor, prevKeys)
  | a when inCurrentPos =>
    switch a {
    | Floor => (Person, prevKeys)
    | Key c => (Person, prevKeys @ [c])
    | Door c => (Person, removeOneFromKeys c prevKeys)
    | tile => (tile, prevKeys)
    }
  | tile => (tile, prevKeys);

let setPosition pos {board, keys} :state => {
  let nextTile = getTile pos board;
  let isNextLegal = isNextPositionLegal keys nextTile;
  let reduceRow y (newRow, prevKeys) tile x => {
    let inCurrentPos = isEqualPos pos (x, y);
    let (newTile, key) = getNextTile inCurrentPos prevKeys tile;
    (newRow @ [newTile], key)
  };
  let reduceColumns (acc, prevKeys) row y => {
    let (newRow, newKeys) = fold_lefti (reduceRow y) ([], prevKeys) row;
    (acc @ [newRow], newKeys)
  };
  if (not isNextLegal) {
    {board, keys}
  } else {
    let (board, keys) = fold_lefti reduceColumns ([], keys) board;
    {board, keys}
  }
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
  (index mod dimension, index / dimension)
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
    printKeyList "Current key inventory" state.keys;
    ReasonReact.Update state
  | _ => ReasonReact.NoUpdate
  };

let board = [
  [Floor, Floor, Floor, Floor, Floor, Floor, Door Red, Floor, Floor],
  [Walls, Walls, Walls, Floor, Walls, Walls, Walls, Walls, Floor],
  [Key Red, Floor, Floor, Floor, Walls, Floor, Key Red, Floor, Floor],
  [Walls, Walls, Walls, Walls, Walls, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Walls, Floor, Floor, Walls, Walls, Walls],
  [Floor, Floor, Floor, Walls, Floor, Walls, Walls, Floor, Floor],
  [Floor, Floor, Floor, Walls, Floor, Walls, Floor, Floor, Floor],
  [Key Red, Walls, Floor, Floor, Floor, Door Red, Door Red, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Walls, Floor, Floor, Floor]
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
