let ft_sum f a b =
	let rec loop f a b acc =
		if a = b then (f a) +. acc 
		else if b < a then nan
		else loop (f) (a + 1) b (acc +. (f a))
	in
	loop f a b 0.0

let eu_dist a b =
	