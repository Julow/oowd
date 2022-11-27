(lang dune 3.4)

(name oowd)

(authors "Jules Aguillon <jules@j3s.fr>")

(source
 (github Julow/oowd))

(license MIT)

(generate_opam_files true)

(package
 (name oowd)
 (synopsis "Turn object-oriented libraries reactive using Lwd.")
 (depends
  (ocaml
   (>= 4.08))
  lwd))

(package
 (name lablgtk3-lwd)
 (synopsis "Reactive bindings to lablgtk3 using Lwd")
 (depends ocaml lwd oowd lablgtk3))
