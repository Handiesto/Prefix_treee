open OUnit2
open Prefix_tree

let test_add _ =
  let open IntListTrie in
  let trie1 = empty in
  let trie2 = add (string_to_int_list "hello") 1 trie1 in
  let trie3 = add (string_to_int_list "world") 2 trie2 in
  assert_equal (find (string_to_int_list "hello") trie1) None ;
  assert_equal (find (string_to_int_list "hello") trie2) (Some 1) ;
  assert_equal (find (string_to_int_list "world") trie2) None ;
  assert_equal (find (string_to_int_list "hello") trie3) (Some 1) ;
  assert_equal (find (string_to_int_list "world") trie3) (Some 2)

let test_add2 _ =
  let open CharListTrie in
  let trie1 = empty in
  let trie2 = add ['h'; 'e'; 'l'; 'l'; 'o'] 1 trie1 in
  assert_equal None (find ['h'; 'e'; 'l'; 'l'; 'o'] trie1) ;
  assert_equal (Some 1) (find ['h'; 'e'; 'l'; 'l'; 'o'] trie2)

let test_add3 _ =
  let open IntListTrie in
  let trie1 = empty in
  let trie2 = add [1; 2; 3] "a" trie1 in
  let trie3 = add [3; 4; 5] "b" trie2 in
  assert_equal None (find [1; 2; 3] trie1) ;
  assert_equal (Some "a") (find [1; 2; 3] trie2) ;
  assert_equal None (find [3; 4; 5] trie2) ;
  assert_equal (Some "a") (find [1; 2; 3] trie3) ;
  assert_equal (Some "b") (find [3; 4; 5] trie3)

let test_remove _ =
  let open IntListTrie in
  let trie1 = empty in
  let trie2 = add (string_to_int_list "hello") 1 trie1 in
  let trie3 = add (string_to_int_list "world") 2 trie2 in
  let trie4 = remove (string_to_int_list "hello") trie3 in
  assert_equal (find (string_to_int_list "hello") trie1) None ;
  assert_equal (find (string_to_int_list "hello") trie2) (Some 1) ;
  assert_equal (find (string_to_int_list "world") trie2) None ;
  assert_equal (find (string_to_int_list "hello") trie3) (Some 1) ;
  assert_equal (find (string_to_int_list "world") trie3) (Some 2) ;
  assert_equal (find (string_to_int_list "hello") trie4) None ;
  assert_equal (find (string_to_int_list "world") trie4) (Some 2)

let test_find _ =
  let open IntListTrie in
  let trie1 = empty in
  let trie2 = add (string_to_int_list "hello") 1 trie1 in
  let trie3 = add (string_to_int_list "world") 2 trie2 in
  let trie4 = add (string_to_int_list "hello") 3 trie3 in
  assert_equal (find (string_to_int_list "hello") trie1) None ;
  assert_equal (find (string_to_int_list "hello") trie2) (Some 1) ;
  assert_equal (find (string_to_int_list "world") trie2) None ;
  assert_equal (find (string_to_int_list "hello") trie3) (Some 1) ;
  assert_equal (find (string_to_int_list "world") trie3) (Some 2) ;
  assert_equal (find (string_to_int_list "hello") trie4) (Some 3) ;
  assert_equal (find (string_to_int_list "world") trie4) (Some 2)

let test_filter _ =
  let open IntListTrie in
  let t = empty in
  let filtered_t = filter (fun _ -> true) t in
  assert (filtered_t = Empty) ;
  let t = add (string_to_int_list "a") 1 empty in
  let filtered_t = filter (fun _ -> true) t in
  assert (isSame t filtered_t) ;
  let t =
    add (string_to_int_list "a") 1
      (add (string_to_int_list "b") 2 (add (string_to_int_list "c") 3 empty))
  in
  let filtered_t = filter (fun _ -> true) t in
  assert (isSame t filtered_t) ;
  let t =
    add (string_to_int_list "a") 1
      (add (string_to_int_list "b") 2 (add (string_to_int_list "c") 3 empty))
  in
  let filtered_t = filter (fun _ -> false) t in
  assert (filtered_t = Empty) ;
  let t =
    add (string_to_int_list "a") 1
      (add (string_to_int_list "b") 2 (add (string_to_int_list "c") 3 empty))
  in
  let filtered_t = filter (fun _ -> true) t in
  assert (isSame t filtered_t)

let test_map _ =
  let open CharListTrie in
  let t1 =
    empty
    |> add ['h'; 'e'; 'l'; 'l'; 'o'] 1
    |> add ['w'; 'o'; 'r'; 'l'; 'd'] 2
  in
  let t2 = map (fun v -> v * 2) t1 in
  assert_equal (Some 1) (find ['h'; 'e'; 'l'; 'l'; 'o'] t1) ;
  assert_equal (Some 2) (find ['w'; 'o'; 'r'; 'l'; 'd'] t1) ;
  assert_equal (Some 2) (find ['h'; 'e'; 'l'; 'l'; 'o'] t2) ;
  assert_equal (Some 4) (find ['w'; 'o'; 'r'; 'l'; 'd'] t2)

let test_merge _ =
  let open IntListTrie in
  let t1 =
    add
      (string_to_int_list "abc")
      1
      (add (string_to_int_list "ab") 2
         (add (string_to_int_list "a") 3 empty) )
  in
  let t2 =
    add
      (string_to_int_list "abc")
      4
      (add (string_to_int_list "a") 5
         (add (string_to_int_list "de") 6 empty) )
  in
  let t3 = merge t1 t2 in
  assert_equal (Some 3) (find (string_to_int_list "a") t3) ;
  assert_equal (Some 2) (find (string_to_int_list "ab") t3) ;
  assert_equal (Some 1) (find (string_to_int_list "abc") t3) ;
  assert_equal (Some 6) (find (string_to_int_list "de") t3) ;
  assert_equal None (find (string_to_int_list "xyz") t3) ;
  assert_equal None (find (string_to_int_list "abcd") t3)

let test_fold_left _ =
  let open IntListTrie in
  let t =
    Node
      ( Some "hello"
      , Map.of_seq
        @@ List.to_seq
             [ (Char.code 'w', Node (None, Map.of_seq @@ List.to_seq []))
             ; ( Char.code 'o'
               , Node (Some "world", Map.of_seq @@ List.to_seq []) ) ] )
  in
  let expected = "helloworld" in
  let actual = fold_left (fun acc x -> acc ^ x) "" t in
  assert (actual = expected)

let test_fold_right _ =
  let open IntListTrie in
  let t =
    Node
      ( Some "hello"
      , Map.of_seq
        @@ List.to_seq
             [ (Char.code 'w', Node (None, Map.of_seq @@ List.to_seq []))
             ; ( Char.code 'o'
               , Node (Some "world", Map.of_seq @@ List.to_seq []) ) ] )
  in
  let expected = "worldhello" in
  let actual = fold_right (fun x acc -> x ^ acc) t "" in
  assert (actual = expected)

let suite =
  "test suite: "
  >::: [ "test_add: " >:: test_add
       ; "test_add2: " >:: test_add2
       ; "test_add3: " >:: test_add3
       ; "test_remove: " >:: test_remove
       ; "test_find: " >:: test_find
       ; "test_filter" >:: test_filter
       ; "test_map" >:: test_map
       ; "test_merge" >:: test_merge
       ; "test_fold_left" >:: test_fold_left
       ; "test_fold_right" >:: test_fold_right ]

let () = run_test_tt_main suite
