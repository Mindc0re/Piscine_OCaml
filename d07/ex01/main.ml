let () =
	let doc = new Doctor.doctor ("Machin") in
	print_endline doc#to_string ;
	doc#talk ;
	doc#travel_in_time 2019 3519 ;
	doc#use_sonic_screwdriver ;
	print_endline doc#to_string