
(* 2A *)


datatype identifier = a | b | c
datatype term = Id of identifier | TermId of term * identifier
datatype expression = Term of term | TermExpression of expression * term


(* 2B *)

val ida = a;
val idb = b;
val idc = c;

val a_times_b = TermId(Id(ida), idb);
val b_times_c = TermId(Id(idb), idc);
val a_times_b_plus_c = TermExpression(Term(a_times_b), Id(idc));
val a_times_b_plus_c_plus_b_times_c = TermExpression(a_times_b_plus_c, b_times_c);
