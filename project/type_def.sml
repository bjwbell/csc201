datatype variable = String_v of string;
type integer_constant = int;
type boolean_constant = bool;
datatype arithmetic_op = Plus | Minus | Times | Divide;
datatype relational_op = eq | lt | le | gt | ge | ne;
datatype boolean_op = And | Or;
datatype constant = Int of integer_constant | Bool of boolean_constant;
datatype int_expression = Cons_Int of constant| Var_Int of variable | Binary_Int of (int_expression*int_expression*arithmetic_op);
datatype bool_expression = Cons_Bool of constant| Var_Bool of variable | Binary_Bool1 of (bool_expression*bool_expression*relational_op) | Binary_Bool2 of (bool_expression*bool_expression*boolean_op);
datatype expression = Int_Expr of int_expression | Bool_Expr of bool_expression;
datatype instruction = Skip | Assign of variable*expression | Compound of instruction list | Conditional of instruction*instruction*expression | Loop of (instruction*expression);
datatype graal_type = Boolean | Integer;
type declaration = variable*graal_type;
type declaration_list = declaration list;
type program = declaration_list*instruction;


val v = String_v("Test");
val b_op = And;
