
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
 
val decl_list : declaration_list = t_decl :: number2_decl :: number1_decl :: nil;
 
 
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
val loop_instructions = number2_assign_expr :: number1_eq_number1_div_10 ::
            t_eq_number1 :: nil;
 
val repeat_loop = Loop(Compound(loop_instructions),
               Binary_Bool1(Var_Int(t), num10, lt));
val conditional = Conditional(Skip, repeat_loop,
                  Binary_Bool1(Var_Int(number1), num0, ge));
 
val instructions : instruction = Compound(conditional :: number2_assign ::
                      number1_assign :: nil);
 
val prog : program = (decl_list, instructions);
 
 
datatype isymbol = Iint | Ibool | Iud;
fun TypeToTypeValue(Boolean) = Ibool | TypeToTypeValue(Integer) = Iint;
 
type typemap = variable -> isymbol;
 
fun TypeMap(nil : declaration_list) = (fn v => Iud) |
    TypeMap((x, t) :: tail) = (fn v => if v = x then TypeToTypeValue(t) else 
				       TypeMap(tail)(v));
 
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

VDeclList([]);
VDeclList([(Variable("a"), Boolean), (Variable("b"), Boolean)]);
VDeclList([(Variable("a"), Boolean), (Variable("b"), Boolean), 
	   (Variable("a"), Boolean)]);

(* VIntExpression *)

fun VIntExpression(Cons_Int(_), _ : typemap) = true |
    VIntExpression(Var_Int(x : variable), map : typemap) = map(x) = Iint |
    VIntExpression(Binary_Int(expr1, expr2, arith_op), map : typemap) = 
    VIntExpression(expr1, map) andalso VIntExpression(expr2, map);

(* code to test VIntExpression *)
VIntExpression(Cons_Int(5), map);
VIntExpression(Var_Int(Variable("x")), map);
VIntExpression(Var_Int(Variable("number1")), map);


(* VBoolExpression *)
fun VBoolExpression(Cons_Bool(_), _ : typemap) = true |
    VBoolExpression(Var_Bool(x : variable), map : typemap) = map(x) = Ibool |
    VBoolExpression(Binary_Bool1(expr1, expr2, rel_op), map : typemap) = 
    VIntExpression(expr1, map) andalso VIntExpression(expr2, map) |
    VBoolExpression(Binary_Bool2(expr1, expr2, bool_op), map : typemap) = 
    VBoolExpression(expr1, map) andalso VBoolExpression(expr2, map);

(* code to test VBoolExpression *)
VBoolExpression(Cons_Bool(true), map);
VBoolExpression(Var_Bool(Variable("x")), map);



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
    VBoolExpression(bool_expr, map)  |
    VInstruction(Loop(instr, bool_expr), map : typemap) = 
    VInstruction(instr, map) andalso VBoolExpression(bool_expr, map);

(* code to test VInstruction *)
(* Empty *)


(* VProgram *)
exception BadDeclationList

fun VProgram(decl_list, instr) = 
    if VDeclList(decl_list) = false then
	raise BadDeclationList
    else
	VInstruction(instr, TypeMap(decl_list));

(* test VProgram with program5 *)
VProgram(prog);
	      
		 
	
