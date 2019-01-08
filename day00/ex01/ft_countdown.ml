let rec ft_countdown n =
	if n <= 0
	then begin print_int 0 ; print_char '\n' end
	else begin print_int n ; print_char '\n' ; (ft_countdown (n - 1)) end

let main () =
	print_endline "Countdown of 5" ; ft_countdown 13 ;
	print_endline "\nCountdown of 0" ; ft_countdown 0 ;
	print_endline "\nCountdown of -42" ; ft_countdown (-42)

let () = main ()