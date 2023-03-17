module CharMap = Map.Make (Char)

type 'a trie = Empty | Node of 'a option * 'a trie CharMap.t

let empty = Empty

let rec add key value trie =
  match (key, trie) with
  | "", _ -> Node (Some value, CharMap.empty)
  | _, Empty ->
      Node
        ( None
        , CharMap.singleton key.[0]
            (add (String.sub key 1 (String.length key - 1)) value Empty) )
  | _, Node (v, children) ->
      let c = key.[0] in
      let cs = String.sub key 1 (String.length key - 1) in
      let subtree = CharMap.find_opt c children in
      let updated_subtree =
        add cs value (Option.value subtree ~default:Empty)
      in
      Node (v, CharMap.add c updated_subtree children)

let rec remove (key : string) (trie : 'a trie) : 'a trie =
  match (key, trie) with
  | "", Node (_, children) -> Node (None, children)
  | _ -> (
      let len = String.length key in
      if len = 0 then trie
      else
        let c = String.get key 0 in
        let cs = String.sub key 1 (len - 1) in
        match trie with
        | Empty -> Empty
        | Node (v, children) -> (
            let subtree =
              try CharMap.find c children with Not_found -> Empty
            in
            let updated_subtree = remove cs subtree in
            match updated_subtree with
            | Empty -> Node (v, CharMap.remove c children)
            | _ -> Node (v, CharMap.add c updated_subtree children) ) )

let rec find (key : string) (trie : 'a trie) : 'a option =
  match (key, trie) with
  | "", Node (value, _) -> value
  | _ -> (
      let len = String.length key in
      if len = 0 then None
      else
        let c = String.get key 0 in
        let cs = String.sub key 1 (len - 1) in
        match trie with
        | Empty -> None
        | Node (_, children) ->
            let subtree =
              try CharMap.find c children with Not_found -> Empty
            in
            find cs subtree )

let rec filter f trie =
  match trie with
  | Empty -> Empty
  | Node (v, children) ->
      let filtered_children =
        CharMap.filter_map
          (fun _ t -> if t = Empty then None else Some (filter f t))
          children
      in
      let filtered_node =
        if f v then Node (v, filtered_children) else Empty
      in
      if CharMap.is_empty filtered_children && filtered_node = Empty then
        Empty
      else filtered_node

let rec isSame t1 t2 =
  match (t1, t2) with
  | Empty, Empty -> true
  | Node (v1, children1), Node (v2, children2) ->
      v1 = v2
      && CharMap.cardinal children1 = CharMap.cardinal children2
      && CharMap.for_all
           (fun k t1' ->
             let t2' = CharMap.find_opt k children2 in
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
      let mapped_children = CharMap.map (map f) children in
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
          CharMap.merge
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
        CharMap.fold (fun _ t' acc'' -> loop acc'' t') m acc'
  in
  loop acc t

let fold_right f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (v, m) -> (
        let acc' = CharMap.fold (fun _ t' acc'' -> loop acc'' t') m acc in
        match v with None -> acc' | Some x -> f x acc' )
  in
  loop acc t
