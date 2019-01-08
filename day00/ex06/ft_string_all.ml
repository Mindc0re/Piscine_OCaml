let ft_string_all func str =
  let i = 0 in
  let len = String.length str in
  let rec loop i continue =
  	if i = len then true
  	else if continue = false then false
  	else if func (String.get str i) then loop (i + 1) true
  	else false
  in
  loop i true

let is_digit c = c >= '0' && c <= '9'

let main () = 
	if ft_string_all is_digit "012345" then print_string "OK"
	else print_string "PAS OK"

let () = main ()