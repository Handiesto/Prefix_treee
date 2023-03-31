module KeyMap = Map.Make (struct
  type t = int

  let compare = compare
end)

type 'a trie = Empty | Node of 'a option * 'a trie KeyMap.t

let empty = Empty

let rec add key value trie =
  match (key, trie) with
  | [], _ -> Node (Some value, KeyMap.empty)
  | _, Empty ->
      Node
        (None, KeyMap.singleton (List.hd key) (add (List.tl key) value Empty))
  | _, Node (v, children) ->
      let c = List.hd key in
      let cs = List.tl key in
      let subtree = KeyMap.find_opt c children in
      let updated_subtree =
        add cs value (Option.value subtree ~default:Empty)
      in
      Node (v, KeyMap.add c updated_subtree children)

let rec remove key trie =
  match (key, trie) with
  | [], Node (_, children) -> Node (None, children)
  | _ -> (
    match trie with
    | Empty -> Empty
    | Node (v, children) -> (
        let c = List.hd key in
        let cs = List.tl key in
        let subtree = try KeyMap.find c children with Not_found -> Empty in
        let updated_subtree = remove cs subtree in
        match updated_subtree with
        | Empty -> Node (v, KeyMap.remove c children)
        | _ -> Node (v, KeyMap.add c updated_subtree children) ) )

let rec find key trie =
  match (key, trie) with
  | [], Node (value, _) -> value
  | _ -> (
    match trie with
    | Empty -> None
    | Node (_, children) ->
        let c = List.hd key in
        let cs = List.tl key in
        let subtree = try KeyMap.find c children with Not_found -> Empty in
        find cs subtree )

let rec filter f trie =
  match trie with
  | Empty -> Empty
  | Node (v, children) ->
      let filtered_children =
        KeyMap.filter_map
          (fun _ t -> if t = Empty then None else Some (filter f t))
          children
      in
      let filtered_node =
        if f v then Node (v, filtered_children) else Empty
      in
      if KeyMap.is_empty filtered_children && filtered_node = Empty then
        Empty
      else filtered_node

let rec isSame t1 t2 =
  match (t1, t2) with
  | Empty, Empty -> true
  | Node (v1, children1), Node (v2, children2) ->
      v1 = v2
      && KeyMap.cardinal children1 = KeyMap.cardinal children2
      && KeyMap.for_all
           (fun k t1' ->
             let t2' = KeyMap.find_opt k children2 in
             match t2' with None -> false | Some t2' -> isSame t1' t2' )
           children1
  | _ -> false

let rec map (f : 'a -> 'b) (trie : 'a trie) : 'b trie =
  match trie with
  | Empty -> Empty
  | Node (value, children) ->
      let mapped_value =
        match value with None -> None | Some v -> Some (f v)
      in
      let mapped_children = KeyMap.map (map f) children in
      Node (mapped_value, mapped_children)

let merge (t1 : 'a trie) (t2 : 'a trie) : 'a trie =
  let merge_node v1 v2 =
    match (v1, v2) with
    | Some _, _ -> v1
    | _, Some _ -> v2
    | None, None -> None
  in
  let rec merge_helper t1 t2 =
    match (t1, t2) with
    | Empty, Empty -> Empty
    | Node (v1, children1), Node (v2, children2) ->
        let merged_value = merge_node v1 v2 in
        let merged_children =
          KeyMap.merge
            (fun _ subtree1 subtree2 ->
              match (subtree1, subtree2) with
              | Some t1, Some t2 -> Some (merge_helper t1 t2)
              | Some t1, None -> Some t1
              | None, Some t2 -> Some t2
              | None, None -> None )
            children1 children2
        in
        Node (merged_value, merged_children)
    | _, Empty -> t1
    | Empty, _ -> t2
  in
  merge_helper t1 t2

let fold_left f acc t =
  let rec loop acc = function
    | Empty -> acc
    | Node (v, m) ->
        let acc' = match v with None -> acc | Some x -> f acc x in
        KeyMap.fold (fun _ t' acc'' -> loop acc'' t') m acc'
  in
  loop acc t

let fold_right f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (v, m) -> (
        let acc' = KeyMap.fold (fun _ t' acc'' -> loop acc'' t') m acc in
        match v with None -> acc' | Some x -> f x acc' )
  in
  loop acc t

let string_to_int_list str =
  let char_list = List.init (String.length str) (String.get str) in
  List.map Char.code char_list

let char_list_to_int_list chars = List.map Char.code chars
