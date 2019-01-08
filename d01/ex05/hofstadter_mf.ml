let rec hfs_f n =
	if n < 0 then (-1)
	else if n = 0 then 1
	else n - hfs_m ( hfs_f (n - 1) )

and hfs_m n =
	if n < 0 then (-1)
	else if n = 0 then 0
	else n - hfs_f( hfs_m(n - 1) )

let () =
	print_string "Testing F with -1 : " ; print_int (hfs_f (-1) ) ;
	print_string "\nTesting F with 0 : " ; print_int (hfs_f 0 );
	print_string "\nTesting F with 7 : " ; print_int (hfs_f 7 );
	print_string "\nTesting F with 9 : " ; print_int (hfs_f 9 );
	print_string "\n\nTesting M with -1 : " ; print_int (hfs_m (-1) ) ;
	print_string "\nTesting M with 0 : " ; print_int (hfs_m 0 );
	print_string "\nTesting M with 2 : " ; print_int (hfs_m 2 );
	print_string "\nTesting M with 3 : " ; print_int (hfs_m 3 );