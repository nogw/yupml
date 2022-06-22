(lang dune 3.0)

(name yupml)

(generate_opam_files true)

(package
 (name yupml)
 (depends 
  (ocaml (and (>= "4.13.0"))) 
  dune
  yojson
  re
  alcotest))