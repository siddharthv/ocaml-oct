## ocaml-oct

Simple library to perform octal conversions.

```ocaml
let s = of_string ?neat:(Some true) "OCaml Rocks!" ;;
val s : [> `Oct of string ] = `Oct "117 103 141 155 154 040 122 157 143 153 163 041"

of_string s ;;
- : string = "OCaml Rocks!"
```
