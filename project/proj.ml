 datatype variable = Variable of string;
type integer_constant = int;
type boolean_constant = bool;
datatype arithmetic_op = Plus | Minus | Times | Divide;
datatype relational_op = eq | lt | le | gt | ge | ne;
datatype boolean_op = And | Or;
datatype int_expression = Cons_Int of integer_constant | Var_Int of variable |
     Binary_Int of (int_expression*int_expression*arithmetic_op);
datatype bool_expression = Cons_Bool of boolean_constant | Var_Bool of variable |
     Binary_Bool1 of (int_expression*int_expression*relational_op) |
     Binary_Bool2 of (bool_expression*bool_expression*boolean_op);
datatype expression = Int_Expr of int_expression | Bool_Expr of bool_expression;
datatype instruction = Skip | Assign of variable*expression |
     Compound of instruction list |
     Conditional of instruction*instruction*bool_expression |
     Loop of (instruction*bool_expression);
datatype graal_type = Boolean | Integer;
type declaration = variable*graal_type;
type declaration_list = declaration list;
type program = declaration_list*instruction;
 
val number1 = Variable("number1");
val number2 = Variable("number2");
val t = Variable("t");
val num10 : int_expression = Cons_Int(10);
val num0 : int_expression = Cons_Int(0);
val num1234567 : int_expression = Cons_Int(1234567);
val number1_decl = (number1, Integer);
val number2_decl = (number2, Integer);
val t_decl = (t, Integer);
 
val decl_list : declaration_list = [t_decl, number2_decl, number1_decl];
 
 
val number1_assign = Assign(number1, Int_Expr(num1234567));
val number2_assign = Assign(number2, Int_Expr(num0));
 
 
 
val t_eq_number1 = Assign(Variable("t"), Int_Expr(Var_Int(number1)));
val number1_div_10 = Binary_Int(Var_Int(number1), num10, Divide);
val number1_eq_number1_div_10 = Assign(number1, Int_Expr(number1_div_10));
val expr1 = Binary_Int(Var_Int(number2), num10, Times);
val expr2 = Binary_Int(Var_Int(number1), num10, Times);
val expr3 = Binary_Int(Var_Int(t), expr2, Minus);
val expr = Binary_Int(expr1, expr3, Plus);
val number2_assign_expr = Assign(number2, Int_Expr(expr));
val loop_instructions = [t_eq_number1, number1_eq_number1_div_10, number2_assign_expr];
val test_cond = Assign(Variable("number2"), Int_Expr(Cons_Int(54)));
val repeat_loop = Loop(Compound(loop_instructions),
               Binary_Bool1(Var_Int(t), num10, lt));
val conditional = Conditional(repeat_loop, Skip,
                  Binary_Bool1(Var_Int(number1), num0, ge));
 
val instructions : instruction = Compound([number1_assign, number2_assign, conditional]);
 
val prog : program = (decl_list, instructions);
 
 
datatype isymbol = Iint | Ibool | Iud;
fun TypeToTypeValue(Boolean) = Ibool | TypeToTypeValue(Integer) = Iint;
 
type typemap = variable -> isymbol;
 
fun TypeMap([]) = (fn v => Iud) |
    TypeMap((x, t) :: tail) = (fn v => if v = x then TypeToTypeValue(t) else TypeMap(tail)(v));
 
(* code to test TypeMap function *)
val map = TypeMap(decl_list);
map(number1);
map(number2);
map(Variable("bad_var"));


(* VDeclList function to validate a declaration list *)

fun ValInHeadNotInTail(var : variable, [] : declaration_list) = true |
    ValInHeadNotInTail(var : variable, ((y, t) :: tail) : declaration_list) =
    (var <> y) andalso ValInHeadNotInTail(var, tail);

fun VDeclList([] : declaration_list) = true |
    VDeclList( (var, t) :: tail : declaration_list) =
    ValInHeadNotInTail(var, tail) andalso VDeclList(tail);

(* code to test VDeclList with a list that has duplicates *)
(* good cases *)
VDeclList([]);
VDeclList([(Variable("a"), Boolean), (Variable("b"), Boolean)]);
VDeclList([(Variable("a"), Boolean), (Variable("b"), Boolean), (Variable("c"), Boolean)]);
(* bad cases *)
VDeclList([(Variable("a"), Boolean), (Variable("b"), Boolean), (Variable("a"), Boolean)]);
VDeclList([(Variable("b"), Boolean), (Variable("a"), Boolean), (Variable("a"), Boolean), (Variable("c"), Boolean)]);
VDeclList([(Variable("a"), Boolean), (Variable("b"), Boolean), (Variable("a"), Boolean), (Variable("c"), Boolean)]);



(* VIntExpression *)

fun VIntExpression(Cons_Int(_), _ : typemap) = true |
    VIntExpression(Var_Int(x : variable), map : typemap) = map(x) = Iint |
    VIntExpression(Binary_Int(expr1, expr2, arith_op), map : typemap) =
    VIntExpression(expr1, map) andalso VIntExpression(expr2, map);

(* code to test VIntExpression *)
(* good cases *) 
VIntExpression(Cons_Int(5), map);
VIntExpression(Var_Int(Variable("number1")), map);
VIntExpression(Binary_Int(Cons_Int(5), Cons_Int(6), Times), map);
VIntExpression(Binary_Int(Var_Int(Variable("number1")), Cons_Int(5), Times), map);
VIntExpression(Binary_Int(Var_Int(Variable("number1")), Var_Int(Variable("number2")), Times), map);
VIntExpression(
		Binary_Int(Binary_Int(Var_Int(Variable("number1")), Cons_Int(3), Plus), 
	       
Binary_Int(Var_Int(Variable("number2")), Cons_Int(1), Times), Times), map);
(* bad cases *)
VIntExpression(Var_Int(Variable("x")), map);
VIntExpression(Binary_Int(Var_Int(Variable("x")), Cons_Int(5), Times), map);
VIntExpression(Binary_Int(Var_Int(Variable("x")), Var_Int(Variable("number2")), Times), map);
VIntExpression(Binary_Int(Var_Int(Variable("x")), Var_Int(Variable("y")), Times), map);

(* VBoolExpression *)
fun VBoolExpression(Cons_Bool(_), _ : typemap) = true |
    VBoolExpression(Var_Bool(x : variable), map : typemap) = map(x) = Ibool |
    VBoolExpression(Binary_Bool1(expr1, expr2, rel_op), map : typemap) =
    VIntExpression(expr1, map) andalso VIntExpression(expr2, map) |
    VBoolExpression(Binary_Bool2(expr1, expr2, bool_op), map : typemap) =
    VBoolExpression(expr1, map) andalso VBoolExpression(expr2, map);

(* code to test VBoolExpression *)
(* good cases *)
VBoolExpression(Cons_Bool(true), map);

VBoolExpression(Binary_Bool1(Cons_Int(5), Cons_Int(4), eq), map);
VBoolExpression(Binary_Bool1(Var_Int(Variable("number1")), Cons_Int(4), eq), map);
VBoolExpression(Binary_Bool1(Var_Int(Variable("number1")), Var_Int(Variable("number2")), eq), map);

val bool1_decl = (Variable("bool1"), Boolean);
val bool2_decl = (Variable("bool2"), Boolean);
 
val bool_decl_list : declaration_list = [bool2_decl, bool1_decl];

val map2 = TypeMap(bool_decl_list);

VBoolExpression(Binary_Bool2(Cons_Bool(true), Cons_Bool(false), Or), map2);
VBoolExpression(Binary_Bool2(Var_Bool(Variable("bool1")), Cons_Bool(true), Or), map2);
VBoolExpression(Binary_Bool2(Var_Bool(Variable("bool2")), Var_Bool(Variable("bool1")), And), map2);

(* bad cases *)
VBoolExpression(Binary_Bool1(Var_Int(Variable("number11")), Cons_Int(4), eq), map);
VBoolExpression(Binary_Bool1(Var_Int(Variable("number11")), Var_Int(Variable("number2")), eq), map);
VBoolExpression(Binary_Bool1(Var_Int(Variable("number11")), Var_Int(Variable("number22")), eq), map);
VBoolExpression(Var_Bool(Variable("x")), map);
VBoolExpression(Binary_Bool2(Var_Bool(Variable("bool11")), Cons_Bool(true), Or), map2);
VBoolExpression(Binary_Bool2(Var_Bool(Variable("bool21")), Var_Bool(Variable("bool1")), And), map2);
VBoolExpression(Binary_Bool2(Var_Bool(Variable("bool21")), Var_Bool(Variable("bool11")), And), map2);


fun VExpression(Int_Expr(expr), map : typemap) = VIntExpression(expr, map) |
    VExpression(Bool_Expr(expr), map : typemap) = VBoolExpression(expr, map);

(* we don't need to test VExpression since it merely delegates to
      VIntExpression and VBoolExpression *)

fun VInstruction(Skip, _ : typemap) = true |
    VInstruction(Assign(x, Int_Expr(exp)), map : typemap) =
    map(x) = Iint andalso VIntExpression(exp, map) |
    VInstruction(Assign(x, Bool_Expr(exp)), map : typemap) =
    map(x) = Ibool andalso VBoolExpression(exp, map) |
    VInstruction(Compound([]), map : typemap) = true |
    VInstruction(Compound(instr :: tail), map : typemap) =
    VInstruction(instr, map) andalso VInstruction(Compound(tail), map) |
    VInstruction(Conditional(instr1, instr2, bool_expr), map : typemap) =
    VInstruction(instr1, map) andalso VInstruction(instr2, map) andalso
    VBoolExpression(bool_expr, map) |
    VInstruction(Loop(instr, bool_expr), map : typemap) =
    VInstruction(instr, map) andalso VBoolExpression(bool_expr, map);

(* code to test VInstruction *)
(* good cases *)
VInstruction(Skip, map);
VInstruction(Assign(Variable("number1"), Int_Expr(Cons_Int(5))), map);
VInstruction(Assign(Variable("bool2"), Bool_Expr(Cons_Bool(true))), map2);
VInstruction(Compound([]), map);
VInstruction(Compound([Assign(Variable("bool2"), Bool_Expr(Cons_Bool(false)))]), map2);
VInstruction(Compound([Assign(Variable("number1"), Int_Expr(Cons_Int(5))), Assign(Variable("number2"), Int_Expr(Cons_Int(4)))]), map);
VInstruction(Conditional(Assign(Variable("number1"), Int_Expr(Cons_Int(5))), Skip, Cons_Bool(true)), map);
VInstruction(Conditional(Skip, Assign(Variable("number1"), Int_Expr(Cons_Int(5))), Cons_Bool(true)), map);
VInstruction(Conditional(Assign(Variable("number2"), Int_Expr(Cons_Int(4))), Assign(Variable("number1"), Int_Expr(Cons_Int(5))), Cons_Bool(true)), map);
VInstruction(Conditional(Skip, Skip, Cons_Bool(true)), map);

VInstruction(Loop(Skip, Cons_Bool(true)), map);
VInstruction(Loop(Skip, Var_Bool(Variable("bool1"))), map2);
VInstruction(Loop(Assign(Variable("bool2"), Bool_Expr(Cons_Bool(false))), Var_Bool(Variable("bool1"))), map2);

(* bad cases *)
VInstruction(Assign(Variable("bad_int_var"), Int_Expr(Cons_Int(5))), map);
VInstruction(Assign(Variable("bad_bool_var"), Bool_Expr(Cons_Bool(true))), map2);
VInstruction(Compound([Assign(Variable("bad_bool1"), Bool_Expr(Cons_Bool(false)))]), map2);
VInstruction(Compound([Assign(Variable("number1"), Int_Expr(Cons_Int(5))), Assign(Variable("bad_number2"), Int_Expr(Cons_Int(4)))]), map);
VInstruction(Conditional(Assign(Variable("bad_number1"), Int_Expr(Cons_Int(5))), Skip, Cons_Bool(true)), map);
VInstruction(Conditional(Skip, Assign(Variable("number1"), Int_Expr(Cons_Int(5))), Var_Bool(Variable("bad_bool"))), map);
VInstruction(Conditional(Assign(Variable("number2"), Int_Expr(Cons_Int(4))), Assign(Variable("bad_num1"), Int_Expr(Cons_Int(5))), Cons_Bool(true)), map);
VInstruction(Conditional(Skip, Skip, Var_Bool(Variable("number1"))), map);

VInstruction(Loop(Skip, Var_Bool(Variable("number1"))), map);
VInstruction(Loop(Skip, Var_Bool(Variable("number1"))), map2);
VInstruction(Loop(Assign(Variable("number1"), Bool_Expr(Cons_Bool(false))), Var_Bool(Variable("bool1"))), map2);


(* VProgram *)
exception BadDeclationList

fun VProgram(decl_list, instr) =
    if VDeclList(decl_list) = false then
raise BadDeclationList
    else
VInstruction(instr, TypeMap(decl_list));

(* test VProgram with program5 *)
VProgram(prog);
(* test VProgram with a bad declaration list *)
val bool1_decl = (Variable("bool1"), Boolean);
val bool11_decl = (Variable("bool1"), Integer);
val bad_decl_list = [bool2_decl, bool11_decl];
val bad_prog : program = (bad_decl_list, instructions);
(*VProgram(bad_prog);*)


(* step 1 *)
datatype value = ValInt of int | ValBool of bool | Unknown;

(* step 2 *)
type programstate = variable -> value;

(* step 3 *)
(*fn v => if v = x then TypeToTypeValue(t) else TypeMap(tail)(v)*)

val ProgramStateChange = (fn (ps : programstate) => (fn (v : variable) => (fn (va : value) => (fn (u : variable) => if u = v then va else ps(u)))));

(* step 4 *)
val initialProgramState = fn (_ : variable) => Unknown;

(* testing initialProgramState *)
print "initialProgramState tests, they should all be false\n";
initialProgramState(Variable("var1"));
initialProgramState(Variable("var2"));

(* testing ProgramStateChange *)
val newProgramState = ProgramStateChange(initialProgramState)(Variable("var"))(ValInt(1));
print "testing ProgramStateChange\n";
newProgramState(Variable("var")) = ValInt(1);
val nps2 = ProgramStateChange(newProgramState)(Variable("var2"))(ValBool(true));
nps2(Variable("var2")) = ValBool(true);
nps2(Variable("var")) = ValInt(1);
nps2(Variable("var3")) = Unknown;

(* step 5 *)
exception BadArithmeticOperation;

fun AOPCalculation(ValInt(int1), Plus, ValInt(int2)) = ValInt(int1 + int2)
| AOPCalculation(ValInt(int1), Minus, ValInt(int2)) = ValInt(int1 - int2)
| AOPCalculation(ValInt(int1), Times, ValInt(int2)) = ValInt(int1 * int2)
| AOPCalculation(ValInt(int1), Divide, ValInt(int2)) = ValInt(int1 div int2)
| AOPCalculation(_, _, _) = raise BadArithmeticOperation;

(* testing AOPCalculation, good cases *)
print "AOPCalculation, good cases\n";
AOPCalculation(ValInt(1), Plus, ValInt(2)) = ValInt(3);
AOPCalculation(ValInt(1), Minus, ValInt(2)) = ValInt(~1);
AOPCalculation(ValInt(4), Times, ValInt(3)) = ValInt(12);
AOPCalculation(ValInt(4), Divide, ValInt(2)) = ValInt(2);

(* testing AOPCalculation, bad cases *)
print "AOPCalculation, bad cases\n";
AOPCalculation(ValBool(false), Plus, ValBool(true)) handle BadArithmeticOperation => ValBool(false);
AOPCalculation(ValInt(1), Times, ValBool(true)) handle BadArithmeticOperation => ValBool(false);
AOPCalculation(ValBool(true), Minus, ValInt(1)) handle BadArithmeticOperation => ValBool(false);



exception BadRelationalOperation;

fun ROPCalculation(ValInt(int1), eq, ValInt(int2)) = ValBool(int1 = int2)
| ROPCalculation(ValInt(int1), lt, ValInt(int2)) = ValBool(int1 < int2)
| ROPCalculation(ValInt(int1), gt, ValInt(int2)) = ValBool(int1 > int2)
| ROPCalculation(ValInt(int1), le, ValInt(int2)) = ValBool(int1 <= int2)
| ROPCalculation(ValInt(int1), ge, ValInt(int2)) = ValBool(int1 >= int2)
| ROPCalculation(ValInt(int1), ne, ValInt(int2)) = ValBool(not (int1 = int2))
| ROPCalculation(_, _, _) = raise BadRelationalOperation;

(* testing ROPCalculation, good cases *)
print "ROPCalculation, good cases\n";
ROPCalculation(ValInt(1), eq, ValInt(1)) = ValBool(true);
ROPCalculation(ValInt(2), eq, ValInt(1)) = ValBool(false);
ROPCalculation(ValInt(1), lt, ValInt(2)) = ValBool(true);
ROPCalculation(ValInt(1), lt, ValInt(1)) = ValBool(false);
ROPCalculation(ValInt(2), lt, ValInt(1)) = ValBool(false);
ROPCalculation(ValInt(1), le, ValInt(2)) = ValBool(true);
ROPCalculation(ValInt(2), le, ValInt(2)) = ValBool(true);
ROPCalculation(ValInt(3), le, ValInt(2)) = ValBool(false);
ROPCalculation(ValInt(1), gt, ValInt(2)) = ValBool(false);
ROPCalculation(ValInt(2), gt, ValInt(2)) = ValBool(false);
ROPCalculation(ValInt(2), gt, ValInt(1)) = ValBool(true);
ROPCalculation(ValInt(3), ge, ValInt(1)) = ValBool(true);
ROPCalculation(ValInt(2), ge, ValInt(2)) = ValBool(true);
ROPCalculation(ValInt(1), ge, ValInt(2)) = ValBool(false);
ROPCalculation(ValInt(1), ne, ValInt(2)) = ValBool(true);
ROPCalculation(ValInt(1), ne, ValInt(1)) = ValBool(false);

(* testing ROPCalculation, bad cases *)
print "ROPCalculation, bad cases\n";
ROPCalculation(ValBool(false), ne, ValBool(true)) handle BadRelationalOperation => ValBool(false);
ROPCalculation(ValInt(1), lt, ValBool(true)) handle BadRelationalOperation => ValBool(false);
ROPCalculation(ValBool(true), gt, ValInt(1)) handle BadRelationalOperation => ValBool(false);


exception BadBooleanOperation;

fun BOPCalculation(ValBool(b1), And, ValBool(b2)) = ValBool(b1 andalso b2)
| BOPCalculation(ValBool(b1), Or, ValBool(b2)) = ValBool(b1 orelse b2)
| BOPCalculation(_, _, _) = raise BadBooleanOperation;

(* testing BOPCalculation, good cases *)
print "BOPCalculation, good cases\n";
BOPCalculation(ValBool(true), And, ValBool(true)) = ValBool(true);
BOPCalculation(ValBool(false), And, ValBool(true)) = ValBool(false);
BOPCalculation(ValBool(false), And, ValBool(false)) = ValBool(false);
BOPCalculation(ValBool(false), Or, ValBool(false)) = ValBool(false);
BOPCalculation(ValBool(false), Or, ValBool(true)) = ValBool(true);
BOPCalculation(ValBool(true), Or, ValBool(true)) = ValBool(true);

(* testing BOPCalculation, bad cases *)
print "BOPCalculation, bad cases\n";
BOPCalculation(ValInt(1), And, ValInt(1)) handle BadBooleanOperation => ValBool(false);
BOPCalculation(ValInt(1), Or, ValBool(true)) handle BadBooleanOperation => ValBool(false);
BOPCalculation(ValBool(true), And, ValInt(1)) handle BadBooleanOperation => ValBool(false);


val rec MIntExpression = fn (Cons_Int(num)) => (fn (ps: programstate) => ValInt(num))
			   | (Var_Int(v : variable)) => (fn (ps : programstate) => ps(v)) 
			   | (Binary_Int(exp1, exp2, aop)) => (fn (ps : programstate) => AOPCalculation(MIntExpression(exp1)(ps), aop, MIntExpression(exp2)(ps)));


(* testing MIntExpression, good cases *)
print "MIntExpression, good cases\n";
MIntExpression(Cons_Int(1))(initialProgramState) = ValInt(1);
MIntExpression(Var_Int(Variable("number1")))(initialProgramState)= Unknown;
val ps2 = ProgramStateChange(initialProgramState)(Variable("number1"))(ValInt(2));
MIntExpression(Var_Int(Variable("number1")))(ps2) = ValInt(2);
MIntExpression(Binary_Int(Cons_Int(1), Cons_Int(2), Plus))(ps2) = ValInt(3);
MIntExpression(Binary_Int(Var_Int(Variable("number1")), Cons_Int(4), Times))(ps2) = ValInt(8);


(* testing MIntExpression, bad cases *)
print "MIntExpression, bad cases\n";
MIntExpression(Binary_Int(Var_Int(Variable("bad_var")), Cons_Int(4), Times))(ps2) handle BadArithmeticOperation => ValBool(false);
MIntExpression(Var_Int(Variable("bad_var")))(ps2) handle BadArithmeticOperation => ValBool(false);


val rec MBoolExpression = fn Cons_Bool(b1) => (fn (ps: programstate) => ValBool(b1) )
		           | Var_Bool(vbool : variable) => (fn (ps: programstate) => ps(vbool) )
			   | Binary_Bool1(iexp1, iexp2, rop) => (fn (ps: programstate) => ROPCalculation(MIntExpression(iexp1)(ps), rop, MIntExpression(iexp2)(ps)))
			   | Binary_Bool2(bexp1, bexp2, bop) => (fn (ps: programstate) => BOPCalculation(MBoolExpression(bexp1)(ps), bop, MBoolExpression(bexp2)(ps)));


(* testing MBoolExpression, good cases *)
print "MBoolExpression, good cases \n";
val ps3 = ProgramStateChange(ps2)(Variable("bool1"))(ValBool(true));

MBoolExpression(Cons_Bool(true))(ps2) = ValBool(true);
MBoolExpression(Var_Bool(Variable("bool1")))(ps3) = ValBool(true);
MBoolExpression(Binary_Bool1(Cons_Int(1), Cons_Int(2), lt))(ps3) = ValBool(true);
MBoolExpression(Binary_Bool1(Var_Int(Variable("number1")), Cons_Int(2), lt))(ps2) = ValBool(false);

MBoolExpression(Binary_Bool2(Cons_Bool(true), Cons_Bool(false), Or))(ps3) = ValBool(true);
MBoolExpression(Binary_Bool2(Var_Bool(Variable("bool1")), Cons_Bool(false), Or))(ps3) = ValBool(true);
MBoolExpression(Binary_Bool2(Var_Bool(Variable("bool1")), Cons_Bool(false), And))(ps3) = ValBool(false);




(* testing MBoolExpression, bad cases *)
print "MBoolExpression, bad cases \n";
MBoolExpression(Var_Bool(Variable("bool2")))(ps3) = ValBool(true);
MBoolExpression(Binary_Bool2(Var_Bool(Variable("bool2")), Cons_Bool(false), And))(ps3) handle BadBooleanOperation => ValBool(false);
MBoolExpression(Binary_Bool1(Var_Int(Variable("bad_var")), Cons_Int(1), gt))(ps3) handle BadRelationalOperation => ValBool(false);



val MExpression = fn Int_Expr(iexpr : int_expression) => (fn (ps: programstate) => MIntExpression(iexpr)(ps)) |
		      Bool_Expr(bexpr : bool_expression) => (fn (ps: programstate) => MBoolExpression(bexpr)(ps));
     

exception BadInstruction;

val rec MInstruction = fn Skip => (fn (ps : programstate) => ps) |
			  Assign (v : variable, e : expression) => (fn (ps : programstate) => ProgramStateChange(ps)(v)(MExpression(e)(ps))) |
			  Compound ([]) => (fn (ps : programstate) => ps) |
			  Compound (x :: xs) => (fn (ps : programstate) => MInstruction(Compound(xs))(MInstruction(x)(ps)))|
			  Conditional (truebranch, falsebranch, test) => (fn (ps : programstate) =>  
									     let val test_result = MBoolExpression(test)(ps)
									     in
										 case test_result of 
										     ValBool(true) => MInstruction(truebranch)(ps) |
										     ValBool(false) => MInstruction(falsebranch)(ps) |
										     _ => raise BadInstruction
												
									     end
									 ) |
			  Loop (loop_instruction, test) => (fn (ps : programstate) => 
							       let val newProgramState = MInstruction(loop_instruction)(ps)
							       in
								   if MBoolExpression(test)(newProgramState) = ValBool(true) then
								       newProgramState
								   else
								       MInstruction(Loop(loop_instruction, test))(newProgramState)
							       end
							   );
(* testing MInstruction, good cases  *)
print "MInstruction good cases\n";
print "Skip\n";
MInstruction(Skip)(initialProgramState);
print "Assign\n";
val ps4 = MInstruction(Assign(Variable("number3"), Int_Expr(Cons_Int(1))))(ps2);
ps4(Variable("number3")) = ValInt(1);
ps4(Variable("bad_var")) = Unknown;
val ps5 = MInstruction(Assign(Variable("bool3"), Bool_Expr(Cons_Bool(true))))(ps4);
ps5(Variable("number3")) = ValInt(1);
ps5(Variable("bool3")) = ValBool(true);
ps5(Variable("bad_var2")) = Unknown;
print "Compound\n";
val psc1 = MInstruction(Compound([]))(ps2);
psc1(Variable("number1")) = ValInt(2);
psc1(Variable("var")) = Unknown;
val assn = Assign(Variable("num2"), Int_Expr(Cons_Int(1)));
val comp = Compound([assn]);
val psc2 = MInstruction(comp)(ps2);
psc2(Variable("number1")) = ValInt(2);
psc2(Variable("num2")) = ValInt(1);
psc2(Variable("var")) = Unknown;
print "Conditional tests\n";
val truebranch1 = Assign(Variable("num2"), Int_Expr(Cons_Int(1)));
val falsebranch1 = Assign(Variable("num2"), Int_Expr(Cons_Int(0)));
val test1 = Cons_Bool(true);
val cond1 = Conditional(truebranch1, falsebranch1, test1);
val pscond1 = MInstruction(cond1)(ps2);
pscond1(Variable("num2")) = ValInt(1);
val test2 = Var_Bool(Variable("b"));
val pstmp = MInstruction(Assign(Variable("b"), Bool_Expr(Cons_Bool(false))))(ps2);
val pscond2 = MInstruction(Conditional(truebranch1, falsebranch1, test2))(pstmp);
pscond2(Variable("b")) = ValBool(false);

print "Loop tests\n";
val loopps = MInstruction(Assign(Variable("loopvar"), Int_Expr(Cons_Int(1))))(ps2);
val loopInstruction = Assign(Variable("loopvar"), Int_Expr(Binary_Int(Var_Int(Variable("loopvar")), Cons_Int(1), Plus)));
val loopTest = Binary_Bool1(Var_Int(Variable("loopvar")), Cons_Int(10), eq);
val loopps2 = MInstruction(Loop(loopInstruction, loopTest))(loopps);
loopps2(Variable("loopvar")) = ValInt(10);
(* testing MInstruction, bad cases *)			 

 
exception BadProgramException;
					   
fun MProgram ((declist, body) : program) = if VProgram(declist, body) then
						MInstruction(body)(initialProgramState) else
						raise BadProgramException;
(* testing MProgram, good cases *)
val finalState = MProgram(prog);
print "finalState number1:";
finalState(Variable("number1"));
print "\n";
print "finalState number2:";
finalState(Variable("number2"));
print "finalState t:";
finalState(Variable("t"));
print "\nfinalState bad_var:";
finalState(Variable("bad_var"));
