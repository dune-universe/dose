let load url =
begin
	let (univ, _, _) = Boilerplate.load_universe [url] in
	univ
end;;
