let my_sleep () = Unix.sleep 1

let main av =
	if (Array.length av) <> 2 then exit 1
	else if ( int_of_string_opt av.(1) ) = None then exit 1
	else let n = int_of_string av.(1) in
	if n <= 0 then exit 1
	else
	for i = (n - 1) downto 0 do 
		my_sleep ()
	done

let () = 
	main Sys.argv