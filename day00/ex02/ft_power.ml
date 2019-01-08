let rec ft_power n pow =
	if n = 0 then 0
	else if pow = 0 then 1
	else n * (ft_power n (pow -1))

let main () = 
	print_int (ft_power 4 0)

let () = main ()