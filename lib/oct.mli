type t = [`Oct of string]

val of_char: char -> char * char * char

val to_char: char -> char -> char -> char

val of_string: ?neat:bool -> string -> t

val to_string: t -> string

