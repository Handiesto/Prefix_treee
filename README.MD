## Лабораторная №2

<b>Выполнил:</b> Плясов Климентий Евгеньевич \
<b>Группа:</b> P34112 \
<b>Преподаватель:</b> Пенской Александр Владимирович

### Префиксное дерево на OCaml

```ocaml
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


```

### Тесты
Unit тесты при помощи OUnit2

```ocaml

open OUnit2
open Prefix_tree

let test_add _ =
  let trie1 = empty in
  let trie2 = add "hello" 1 trie1 in
  let trie3 = add "world" 2 trie2 in
  let trie4 = add "hello" 3 trie3 in
  assert_equal (find "hello" trie1) None;
  assert_equal (find "hello" trie2) (Some 1);
  assert_equal (find "world" trie2) None;
  assert_equal (find "hello" trie3) (Some 1);
  assert_equal (find "world" trie3) (Some 2);
  assert_equal (find "hello" trie4) (Some 3);
  assert_equal (find "world" trie4) (Some 2)

let test_remove _ =
  let trie1 = empty in
  let trie2 = add "hello" 1 trie1 in
  let trie3 = add "world" 2 trie2 in
  let trie4 = remove "hello" trie3 in
  assert_equal (find "hello" trie1) None;
  assert_equal (find "hello" trie2) (Some 1);
  assert_equal (find "world" trie2) None;
  assert_equal (find "hello" trie3) (Some 1);
  assert_equal (find "world" trie3) (Some 2);
  assert_equal (find "hello" trie4) None;
  assert_equal (find "world" trie4) (Some 2)


let test_find _ =
  let trie1 = empty in
  let trie2 = add "hello" 1 trie1 in
  let trie3 = add "world" 2 trie2 in
  let trie4 = add "hello" 3 trie3 in
  assert_equal (find "hello" trie1) None;
  assert_equal (find "hello" trie2) (Some 1);
  assert_equal (find "world" trie2) None;
  assert_equal (find "hello" trie3) (Some 1);
  assert_equal (find "world" trie3) (Some 2);
  assert_equal (find "hello" trie4) (Some 3);
  assert_equal (find "world" trie4) (Some 2)


let test_filter _ =
  let t1 = empty in
  let t2 = add "apple" 1 t1 in
  let t3 = add "banana" 2 t2 in
  let t4 = add "orange" 3 t3 in
  let t5 = filter (fun key value -> String.length key > 5 || value = Some 2) t4 in
  assert_equal None (find "apple" t5);
  assert_equal (Some 2) (find "banana" t5);
  assert_equal None (find "orange" t5)

let test_map _ =
  let t1 = empty |> add "hello" 1 |> add "world" 2 in
  let t2 = map (fun v -> v * 2) t1 in
  assert_equal (Some 1) (find "hello" t1);
  assert_equal (Some 2) (find "world" t1);
  assert_equal (Some 2) (find "hello" t2);
  assert_equal (Some 4) (find "world" t2)

let test_merge _ =
  let t1 = add "abc" 1 (add "ab" 2 (add "a" 3 empty)) in
  let t2 = add "abc" 4 (add "a" 5 (add "de" 6 empty)) in
  let t3 = merge t1 t2 in
  assert_equal (Some 3) (find "a" t3);
  assert_equal (Some 2) (find "ab" t3);
  assert_equal (Some 1) (find "abc" t3);
  assert_equal (Some 6) (find "de" t3);
  assert_equal None (find "xyz" t3);
  assert_equal None (find "abcd" t3)

let test_fold_left _ =
  let t =
    Node (Some "hello",
      CharMap.of_seq @@ List.to_seq
        [ ('w', Node (None, CharMap.of_seq @@ List.to_seq []))
        ; ('o', Node (Some "world", CharMap.of_seq @@ List.to_seq []))
        ]
    )
  in
  let expected = "helloworld" in
  let actual = fold_left (fun acc x -> acc ^ x) "" t in
  assert (actual = expected)

let test_fold_right _ =
  let t =
    Node (Some "hello",
      CharMap.of_seq @@ List.to_seq
        [ ('w', Node (None, CharMap.of_seq @@ List.to_seq []))
        ; ('o', Node (Some "world", CharMap.of_seq @@ List.to_seq []))
        ]
    )
  in
  let expected = "helloworld" in
  let actual = fold_right (fun x acc -> x ^ acc) t "" in
  assert (actual = expected)


let suite =
  "test suite: " >::: [
    "test_add: " >:: test_add;
    "test_remove: " >:: test_remove;
    "test_find: " >:: test_find;
    "test_filter" >:: test_filter;
    "test_map"    >:: test_map;
    "test_merge"  >:: test_merge;
    "test_fold_left" >:: test_fold_left;
    "test_fold_right" >:: test_fold_right
  ]

let () =
  run_test_tt_main suite;




```
........ 
Ran: 8 tests in: 0.11 seconds.
OK



### Выводы
Property Based Testing - позволяет автоматически генерировать большое количество тестовых случаев на основе свойств или требований. Были протестированны такие свойства моноида как: ассоциативность, наличие нейтрального элемента, свойства вставки и удаления, которые сохраняют упорядоченность элементов в дереве. В ходе лабораторной работы я научилися использовать Property Based Testing и узнал о существовании monoid