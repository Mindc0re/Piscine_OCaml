let getJokes fileName =
	let jokeList = ref [] in
	let file = open_in fileName in
	begin
		try
			while true do
				jokeList := input_line file :: !jokeList
			done
		with 
		| End_of_file -> ()
	end ;
	close_in file ;
	!jokeList


let () =
	Random.self_init () ;
	let av = Sys.argv in
	if Array.length av <> 2 then exit 1
	else let jokes = Array.of_list ( getJokes av.(1) ) in
	if Array.length jokes > 0 then print_endline jokes.(Random.int (Array.length jokes) )