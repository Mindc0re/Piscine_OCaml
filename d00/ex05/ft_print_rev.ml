let ft_print_rev str = 
	let len = ((String.length str) - 1) in
	let rec loop len =
		if len < 0 then print_char '\n'
		else
		begin
			print_char (String.get str len) ;
			loop (len - 1)
		end
	in
	loop len

let main () =
	print_string "Reversing \"Hello world !\" : " ;
	ft_print_rev "Hello world !" ;

	print_string "Reversing \"42\" : " ;
	ft_print_rev "42" ;

	print_string "Reversing \"\" : " ;
	ft_print_rev "" ;

	print_string "Reversing \"a\" : " ;
	ft_print_rev "a"

let () = main ()