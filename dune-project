(lang dune 3.20)

(name berkeley-cs164-arm-darwin)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Qiu Xuanluo <owobit@foxmail.com>")

(maintainers "Qiu Xuanluo <owobit@foxmail.com>")

(license LICENSE)

(documentation https://github.com/xuanluoya/berkeley-cs164-arm-darwin)

(package
 (name berkeley-cs164-arm-darwin)
 (synopsis "A simple Lisp compiler for teaching runs on Apple Arm64")
 (description "This repository is a reconstruction of the Apple Arm architecture of the CS164-2025 programming language and compiler course at the University of California, Berkeley.")
 (depends ocaml)
 (tags
  ("Compiler" "Tutorial")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
