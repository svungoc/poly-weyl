(lang dune 1.11)
(name math)
(version "0.01")
(generate_opam_files true)
(license GPL2)
(maintainers "Vu Ngoc San <san.vu-ngoc@univ-rennes1.fr>")
(authors "Vu Ngoc San <san.vu-ngoc@univ-rennes1.fr>")
(source (github svungoc/math))

(package
 (name math)
 (synopsis "Various basic mathematical algebraic structures")
 (depends
  (zarith (>= 1.9.1))))