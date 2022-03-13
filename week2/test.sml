
exception ListLengthMismatch
(* don't do this *)
fun shallow_zip3 (l1,l2,l3) =
    case l1 of
	[] => 
	(case l2 of 
	     [] => (case l3 of
			[] => []
		      | _ => raise ListLengthMismatch)
	   | _ => raise ListLengthMismatch)
      | hd1::tl1 => 
	(case l2 of
	     [] => raise ListLengthMismatch
	   | hd2::tl2 => (case l3 of
			      [] => raise ListLengthMismatch
			    | hd3::tl3 => 
			      (hd1,hd2,hd3)::shallow_zip3(tl1,tl2,tl3)))