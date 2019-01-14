let () =
	Random.self_init () ;
	let jokes =	[|
					"Qu'est ce qui est jaune et qui attend ? Jonathan. Ils ont dit que tu devais avoir envie de me baffer pour avoir des points en plus." ;
					"What's yellow and waiting ? Jonath.. Crap, this doesn't work in english" ;
					"!false; It's funny 'cause it's true." ;
					"I’ve been hearing news about this big boolean. Huge if true." ;
					"What's the object-oriented way to become wealthy? Inheritance."
				|] in
	print_endline jokes.(Random.int 5)