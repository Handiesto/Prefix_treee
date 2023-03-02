module CharMap = Map.Make(Char)

type 'a trie = Empty | Node of 'a option * ('a trie) CharMap.t

let empty = Empty

let rec add (key : string) (value : 'a) (trie : 'a trie) : 'a trie =
  match key with
  | "" -> Node(Some value, CharMap.empty)
  | _ ->
    let len = String.length key in
    if len = 0 then
      trie
    else
      let c = String.get key 0 in
      let cs = String.sub key 1 (len - 1) in
      match trie with
      | Empty -> Node(None, CharMap.singleton c (add cs value Empty))
      | Node(v, children) ->
        let subtree = try CharMap.find c children with Not_found -> Empty in
        let updated_subtree = add cs value subtree in
        Node(v, CharMap.add c updated_subtree children)



let rec remove (key : string) (trie : 'a trie) : 'a trie =
  match key, trie with
  | "", Node(_, children) -> Node(None, children)
  | _ ->
    let len = String.length key in
    if len = 0 then
      trie
    else
      let c = String.get key 0 in
      let cs = String.sub key 1 (len - 1) in
      match trie with
      | Empty -> Empty
      | Node(v, children) ->
        let subtree = try CharMap.find c children with Not_found -> Empty in
        let updated_subtree = remove cs subtree in
        match updated_subtree with
        | Empty -> Node(v, CharMap.remove c children)
        | _ -> Node(v, CharMap.add c updated_subtree children)


let rec find (key : string) (trie : 'a trie) : 'a option =
  match key, trie with
  | "", Node(value, _) -> value
  | _ ->
    let len = String.length key in
    if len = 0 then
      None
    else
      let c = String.get key 0 in
      let cs = String.sub key 1 (len - 1) in
      match trie with
      | Empty -> None
      | Node(_, children) ->
        let subtree = try CharMap.find c children with Not_found -> Empty in
        find cs subtree



let rec filter (predicate : string -> 'a option -> bool) (trie : 'a trie) : 'a trie =
  match trie with
  | Empty -> Empty
  | Node(value, children) ->
    let filtered_children = CharMap.filter_map
      (fun _ subtree -> Some (filter predicate subtree))
      children in
    let filtered_value = match value with
      | None -> None
      | Some v -> if predicate "" (Some v) then Some v else None in
    if CharMap.is_empty filtered_children && filtered_value = None then
      Empty
    else
      Node(filtered_value, filtered_children)


let rec map (f : 'a -> 'b) (trie : 'a trie) : 'b trie =
  match trie with
  | Empty -> Empty
  | Node(value, children) ->
    let mapped_value = match value with
      | None -> None
      | Some v -> Some (f v) in
    let mapped_children = CharMap.map (map f) children in
    Node(mapped_value, mapped_children)


let merge (t1 : 'a trie) (t2 : 'a trie) : 'a trie =
  let merge_node v1 v2 =
    match v1, v2 with
    | Some _, _ -> v1
    | _, Some _ -> v2
    | None, None -> None
  in
  let rec merge_helper t1 t2 =
    match t1, t2 with
    | Empty, Empty -> Empty
    | Node(v1, children1), Node(v2, children2) ->
      let merged_value = merge_node v1 v2 in
      let merged_children = CharMap.merge
        (fun _ subtree1 subtree2 ->
          match subtree1, subtree2 with
          | Some t1, Some t2 -> Some (merge_helper t1 t2)
          | Some t1, None -> Some t1
          | None, Some t2 -> Some t2
          | None, None -> None)
        children1 children2
      in
      Node(merged_value, merged_children)
    | _, Empty -> t1
    | Empty, _ -> t2
  in
  merge_helper t1 t2


let fold_left f acc t =
  let rec loop acc = function
    | Empty -> acc
    | Node (v, m) ->
      let acc' = match v with
        | None -> acc
        | Some x -> f acc x
      in
      CharMap.fold (fun _ t' acc'' -> loop acc'' t') m acc'
  in
  loop acc t


let fold_right f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (v, m) ->
      let acc' = CharMap.fold (fun _ t' acc'' -> loop acc'' t') m acc in
      match v with
      | None -> acc'
      | Some x -> f x acc'
  in
  loop acc t
