(** architecture strings ************************************************)

let architecture_test_cases = [
  ("all", "i386", true);               (* all matches everything *)
  ("any", "kfreebsd-amd64",true);      (* any matches everything *)
  ("amd64", "i386", false);            (* pattern and arch do not split *)
  ("toaster", "toaster", true);        
  ("hurd-amd64", "hurd-amd64", true);  (* pattern and arch split *)
  ("hurd-amd64", "netbsd-amd64", false);   
  ("hurd-amd64", "hurd-i386", false);
  ("hurd-amd64", "netbsd-i386", false);
  ("hurd-amd64", "amd64", false);      (* pattern splits, arch doesn't *)
  ("hurd-amd64", "i386", false);
  ("linux-amd64", "amd64", true);
  ("linux-amd64", "i386", false);
  ("amd64", "hurd-amd64", false);      (* arch splits,patten doesn't *)
  ("amd64", "hurd-i386", false);
  ("amd64", "linux-amd64", true);
  ("amd64", "linux-i386", false);
  ("any-amd64", "hurd-amd64", true);   (* OS pattern *)
  ("any-amd64", "linux-amd64", true);
  ("any-amd64", "hurd-i386", false);
  ("any-amd64", "linux-i386", false);
  ("hurd-any", "hurd-alpha", true);    (* CPU pattern *)
  ("linux-any", "linux-alpha", true);
  ("hurd-any", "netbsd-alpha", false);
  ("linux-any", "netbsd-alpha", false);
  ("any-any", "linux-i386", true);     (* OS and CPU pattern *)
  ("any-any", "hurd-i386", true);
  ("any-any", "amd64", true)
];;

List.iter
  (fun (source,arch,expected) ->
    let result = src_matches_arch source arch  in
    if result <> expected
    then
      begin
	Printf.printf "error matching architecture %s against %s\n" source arch;
	Printf.printf "found %b, should be %b\n" result expected
      end;
  )
  architecture_test_cases
;;
