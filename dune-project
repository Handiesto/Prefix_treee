(lang dune 2.9)

(name prefix_tree)

(generate_opam_files true)

(source
 (github TimeToStop/binary_tree))

(authors "Plyasov K. E.")

(maintainers "Plyasov K. E.")

(license LICENSE)

(documentation https://github.com/TimeToStop/binary_tree)

(package
 (name prefix_tree)
 (synopsis "Lab2")
 (description "Prefix tree in ocaml")
 (depends ocaml dune ounit2 qcheck))