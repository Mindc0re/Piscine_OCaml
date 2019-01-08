let ft_string_all func str =
  let i = 0 in
  let len = String.length str in
  let rec loop i continue =
  	if i = len && len <> 0 then true
  	else if continue = false || len = 0 then false
  	else if func (String.get str i) then loop (i + 1) true
  	else false
  in
  loop i true

let is_digit c = c >= '0' && c <= '9'

let is_42 c = c = '4' || c = '2'

let main () = 
  print_string "Testing with is_digit and \"012345\" : " ;
	if ft_string_all is_digit "012345" then print_endline "true" else print_endline "false" ;

  print_string "Testing with is_digit and \"01b345\" : " ;
  if ft_string_all is_digit "01b345" then print_endline "true" else print_endline "false" ;

  print_string "Testing with is_42 and \"42\" : " ;
  if ft_string_all is_42 "42" then print_endline "true" else print_endline "false" ;

  print_string "Testing with is_42 and \"42blablabla\" : " ;
  if ft_string_all is_42 "42blablabla" then print_endline "true" else print_endline "false" ;

  print_string "Testing with is_42 and \"\" : " ;
  if ft_string_all is_42 "" then print_endline "true" else print_endline "false" 

let () = main ()