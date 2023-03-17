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



(* Test for empty trie *)
let test_filter _ =
  let t = empty in
  let filtered_t = filter (fun _ -> true) t in
  assert (filtered_t = Empty);

  let t = add "a" 1 empty in
  let filtered_t = filter (fun _ -> true) t in
  assert (isSame t filtered_t);

  let t = add "a" 1 (add "b" 2 (add "c" 3 empty)) in
  let filtered_t = filter (fun _ -> true) t in
  assert (isSame t filtered_t);
  let t = add "a" 1 (add "b" 2 (add "c" 3 empty)) in
  let filtered_t = filter (fun _ -> false) t in
  assert (filtered_t = Empty);
  let t = add "a" 1 (add "b" 2 (add "c" 3 empty)) in
  let filtered_t = filter (fun _ -> true) t in
  assert (isSame t filtered_t)


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


