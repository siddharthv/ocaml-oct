
type t = [`Oct of string]

let invalid_arg fmt =
	Printf.ksprintf (fun str -> raise (Invalid_argument str)) fmt

let oct = "01234567"

let decimal_to_octal (x:int) =
	let rec d2o y lst = match y with
	|0|1|2|3|4|5|6|7-> y :: lst
	| _ -> d2o (y/8) ((y mod 8) :: lst)
	in
	d2o x []

let of_char c =
	let x = Char.code c in
	let ol = decimal_to_octal x
	in
	match List.length ol with
	| 1 -> '0', '0', oct.[List.nth ol 0]
	| 2 -> '0', oct.[List.nth ol 0], oct.[List.nth ol 1]
	| 3 -> oct.[List.nth ol 0], oct.[List.nth ol 1], oct.[List.nth ol 2]
	| _ -> invalid_arg "incorrect state"

let of_string ?(neat=false)s =
	let n = String.length s in
	let buf = Buffer.create (n*3) in
	for i = 0 to n-1 do
		let x, y, z = of_char s.[i] in
		Buffer.add_char buf x;
		Buffer.add_char buf y;
		Buffer.add_char buf z;
		if neat then
			if i+1 <> n then Buffer.add_char buf ' '
	done;
	`Oct (Buffer.contents buf)

let can_skip = function
	| ' ' | '\t' | '\n' | '\r' | '-' -> true
	| _ -> false

let to_char x y z =
	let code c = match c with
	| '0'..'7' -> Char.code c - 48
	| _ -> invalid_arg "Oct.of_char: %d is an invalid char" (Char.code c)
	in
	Char.chr (code x lsl 6 + code y lsl 3 + code z)

let to_string (`Oct s) =
	if s = "" then ""
	else
		let n = String.length s in
		let buf = Buffer.create (1 + n/3) in
		let rec aux i j k =
			if i >= n then ()
			else if can_skip s.[i] then aux (i+1) (i+2) (i+3)
			else if j >= n then invalid_arg "Oct.to_string: invalid octal string"
			else if can_skip s.[j] then aux i (j+1) (j+2)
			else if k >= n then invalid_arg "Oct.to_string: invalid octal string"
			else if can_skip s.[k] then aux i j (k+1)
			else (
				Buffer.add_char buf (to_char s.[i] s.[j] s.[k]);
				aux (k+1) (k+2) (k+3)
			)
		in
		aux 0 1 2;
		Buffer.contents buf
