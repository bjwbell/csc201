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

