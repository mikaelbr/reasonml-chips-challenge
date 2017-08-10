module type Comparable = {type t; let equal: t => t => bool;};

module MakeSet (Item: Comparable) => {
  type backingType = list Item.t;
  let empty = [];
  let add (currentSet: backingType) (newItem: Item.t) :backingType =>
    List.exists (Item.equal newItem) currentSet ? currentSet : [newItem, ...currentSet];
};
