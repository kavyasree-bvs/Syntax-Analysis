/* File: parser.y
 * --------------
 * Yacc input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should 
 *      accept the language as described in specification, and as augmented 
 *      in the pp2 handout.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine

%}

/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */
 
/* yylval 
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser. 
 *
 * pp2: You will need to add new fields to this union as you add different 
 *      attributes to your non-terminal symbols.
 */
%union {
    int integerConstant;
    bool boolConstant;
    char *stringConstant;
    double doubleConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    Decl *decl;
    List<Decl*> *declList;
	
	VarDecl *varDecl;
	ClassDecl *classDecl;
	InterfaceDecl *interfaceDecl;
	FnDecl *fnDecl;
	Type *type;
	
	List<VarDecl*> *varList;
	List<NamedType*> *namedTypeList;
	NamedType *namedType;
	StmtBlock *stmtBlock;
	List<Stmt*> *stmtList;
	Stmt *stmt;
	
	Expr *expr;
	List<Expr*> *exprList;

	
	SwitchStmt *switchStmt;
	CaseStmt *caseStmt;
	DefaultStmt *defaultStmt;
	List<CaseStmt*> *caseStmtList;


}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Yacc will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */
%token   T_Void T_Bool T_Int T_Double T_String T_Class 
%token   T_LessEqual T_GreaterEqual T_Equal T_NotEqual T_Dims
%token   T_And T_Or T_Null T_Extends T_This T_Interface T_Implements
%token   T_While T_For T_If T_Else T_Return T_Break
%token   T_New T_NewArray T_Print T_ReadInteger T_ReadLine
%token   T_Increment T_Decrement T_Switch T_Case T_Default T_Colon

%token   <identifier> T_Identifier
%token   <stringConstant> T_StringConstant 
%token   <integerConstant> T_IntConstant
%token   <doubleConstant> T_DoubleConstant
%token   <boolConstant> T_BoolConstant


/*
[ . (array indexing and field selection)
! - (unary -, logical not)
** / % (multiply, divide, mod)
+ - (addition, subtraction)
< <= > >= (relational)
== != (equality)
&& (logical and)
|| (logical or)
= (assignment)
*/

%left '='
%left T_Or
%left T_And
%nonassoc T_Equal T_NotEqual 
%nonassoc '<' T_LessEqual '>' T_GreaterEqual
%left '+' '-'
%nonassoc T_Increment T_Decrement
%left '*' '/' '%'
%nonassoc unaryMinus '!' 
%nonassoc '[' '.'






/* Non-terminal types
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */
%type <declList>  DeclList FieldList PrototypeList
%type <decl>      Decl Field Prototype
%type <varDecl>	VariableDecl
%type <classDecl>	ClassDecl
%type <interfaceDecl>	InterfaceDecl
%type <fnDecl>	FunctionDecl
%type <varDecl> Variable
%type <type> Type
%type <varList> Formals CSFormalList VariableDeclList
%type <namedTypeList> ImplementsIdentList IdentList
%type <namedType> ExtendsIdent
%type <stmtBlock> StmtBlock
%type <stmtList> StmtList
%type <stmt> IfStmt ForStmt WhileStmt BreakStmt ReturnStmt PrintStmt ElseStmt Stmt
%type <expr> Expr OptionalExpr Call Constant LValue
%type <exprList> NonEmptyExprList Actuals 
%type <switchStmt> SwitchStmt
%type <caseStmt> CaseStmt
%type <defaultStmt> DefaultStmt
%type <caseStmtList> CaseStmtsList

%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.
	 
 */
Program   :    DeclList            { 
                                      @1; 
                                      /* pp2: The @1 is needed to convince 
                                       * yacc to set up yylloc. You can remove 
                                       * it once you have other uses of @n*/
                                      Program *program = new Program($1);
                                      // if no errors, advance to next phase
                                      if (ReportError::NumErrors() == 0) 
                                          program->Print(0);
                                    }
          ;

DeclList  :    DeclList Decl        { ($$=$1)->Append($2); }
          |    Decl                 { ($$ = new List<Decl*>)->Append($1); }
          ;

		  
Decl 	:	VariableDecl	{$$=$1;}
		| FunctionDecl		{$$=$1;}
		| ClassDecl			{$$=$1;}
		| InterfaceDecl		{$$=$1;}
		;

VariableDecl	: Variable ';' {$$=$1;}
				;

Variable		: Type T_Identifier {
										/* VarDecl(Identifier *name, Type *type)*/
										/*Identifier(yyltype loc, const char *name);*/
										$$=new VarDecl(new Identifier(@2,$2), $1);
									}
				;
				
Type 			: T_Int	{$$=Type::intType;} 
				| T_Double {$$=Type::doubleType;}
				| T_Bool {$$=Type::boolType;}
				| T_String {$$=Type::stringType;}
				| T_Identifier	{
									/*NamedType(Identifier *i);*/
									$$=new NamedType(new Identifier(@1,$1));
								}
				| Type T_Dims	{
									/*ArrayType(yyltype loc, Type *elemType);*/
									/* Function: Join
									 * --------------
									 * Takes two locations and returns a new location which represents
									 * the span from first to last, inclusive.
									 */
									$$=new ArrayType(Join(@1,@2), $1);
								}
				;
				
FunctionDecl	: Type T_Identifier '(' Formals ')' StmtBlock	{
																	/*FnDecl(Identifier *name, Type *returnType, List<VarDecl*> *formals);*/
																	$$=new FnDecl(new Identifier(@2,$2),$1,$4);
																	/*void SetFunctionBody(Stmt *b);*/
																	$$->SetFunctionBody($6);
																}
				| T_Void T_Identifier '(' Formals ')' StmtBlock	{
																	$$=new FnDecl(new Identifier(@2,$2),Type::voidType,$4);
																	$$->SetFunctionBody($6);
																}
																
			;
			
Formals : 	CSFormalList{	
							/*Variable+,*/
							/*x+, a comma-separated list of one or more x’s (commas appear only between x’s)*/
							$$=$1;
						}
			| /*epsilon*/ {	
						/*epsilon, the absence of tokens*/
						$$=new List<VarDecl*>; /*why this?*/
					}
			;
			
CSFormalList:	CSFormalList ',' Variable {($$=$1)->Append($3);}
				| Variable	{($$=new List<VarDecl*>)->Append($1);}
				;
				
/*ClassDecl ::= class ident < extends ident> <implements ident+,> { Field* }*/
/*<x> means zero or one occurrence of x, i.e., x is optional*/
ClassDecl :	T_Class T_Identifier ExtendsIdent ImplementsIdentList '{' FieldList '}' {
																						/*ClassDecl(Identifier *name, $3, $4, $6);*/
																						$$=new ClassDecl(new Identifier(@2,$2), $3, $4, $6);
																					}
			;

ExtendsIdent : T_Extends T_Identifier {$$=new NamedType(new Identifier(@2,$2));}
			| /*empty*/ {$$=NULL;}
			;

ImplementsIdentList	: T_Implements IdentList {$$=$2;}
					| /*empty*/ {$$=new List<NamedType*>;}
					;

IdentList : IdentList ',' T_Identifier {($$=$1)->Append(new NamedType(new Identifier(@3,$3)));}
			| T_Identifier {($$=new List<NamedType*>)->Append(new NamedType(new Identifier(@1,$1)));}
			;

FieldList 	: FieldList Field {($$=$1)->Append($2);}
			| /* empty */{$$=new List<Decl*>();}
			;
			
Field 	: VariableDecl{$$=$1;}
		| FunctionDecl{$$=$1;}
		;

InterfaceDecl	: T_Interface T_Identifier '{' PrototypeList '}' {$$=new InterfaceDecl(new Identifier(@2,$2),$4);}
				;

PrototypeList	: PrototypeList Prototype {($$=$1)->Append($2);} 
				| /*empty*/ {$$=new List<Decl*>;}
				;

/*Prototype ::= Type ident ( Formals ) ; | void ident ( Formals ) ;*/
Prototype	: Type T_Identifier '(' Formals ')' ';' {$$=new FnDecl(new Identifier(@2,$2), $1, $4);}
			| T_Void T_Identifier '(' Formals ')' ';' {$$=new FnDecl(new Identifier(@2,$2), Type::voidType, $4);}
			;

/*StmtBlock ::= { VariableDecl* Stmt* }*/
StmtBlock	: '{' VariableDeclList StmtList '}' {$$=new StmtBlock($2,$3);}
			;
			
VariableDeclList	: VariableDeclList VariableDecl {($$=$1)->Append($2);}
					| /*empty*/ {$$=new List<VarDecl*>;}
					;

StmtList	: Stmt StmtList {$$=$2; $$->InsertAt($1,0);}
			| /*empty*/ {$$=new List<Stmt*>;}
			;

/*Stmt ::= <Expr>; | IfStmt | WhileStmt | ForStmt | BreakStmt |ReturnStmt | PrintStmt | StmtBlock*/
Stmt	: OptionalExpr ';' {$$=$1;}
		| IfStmt {$$=$1;}
		| WhileStmt {$$=$1;}
		| ForStmt {$$=$1;}
		| BreakStmt {$$=$1;}
		| ReturnStmt {$$=$1;}
		| PrintStmt {$$=$1;}
		| StmtBlock {$$=$1;}
		| SwitchStmt {$$=$1;}
		;
		
OptionalExpr	: 	Expr {$$=$1;}
				| /*empty*/ {$$=new EmptyExpr();}
				;
				
/*IfStmt ::= if ( Expr ) Stmt <else Stmt>*/		
IfStmt	: T_If '(' Expr ')' Stmt ElseStmt {$$=new IfStmt($3, $5, $6);}
		;

/*<else Stmt>*/
ElseStmt	: T_Else Stmt {$$=$2;}
			| /*optional else*/ {$$=NULL;}
			;

/*WhileStmt ::= while ( Expr ) Stmt*/					
WhileStmt	: T_While '(' Expr ')' Stmt {$$=new WhileStmt($3, $5);}
			;
/*ForStmt ::= for ( <Expr>; Expr ; <Expr>) Stmt*/
ForStmt		: T_For '(' OptionalExpr ';' Expr ';' OptionalExpr')' Stmt {$$=new ForStmt($3, $5, $7, $9);}
			;
			
/*BreakStmt ::= break ;*/
BreakStmt	: T_Break ';' {$$=new BreakStmt(Join(@1, @2));}
			;

/*ReturnStmt ::= return < Expr > ;*/
ReturnStmt	: T_Return OptionalExpr ';' {$$=new ReturnStmt(Join(@1, @3), $2);}
			;

/*PrintStmt ::= Print ( Expr+, ) ;*/
PrintStmt	: T_Print '(' NonEmptyExprList ')' ';' {$$=new PrintStmt($3);}
			;

NonEmptyExprList	: NonEmptyExprList ',' Expr {($$=$1)->Append($3);}
					| Expr {($$=new List<Expr*>)->Append($1);}
					;

/*SwitchStmt ::= switch ( Expr) { cases default}*/
SwitchStmt	: T_Switch '(' Expr ')' '{' CaseStmtsList DefaultStmt '}' {$$=new SwitchStmt($3,$6,$7);}
			;

CaseStmtsList	: CaseStmtsList CaseStmt {($$=$1)->Append($2);}
            	| CaseStmt {($$=new List<CaseStmt*>)->Append($1);}
            	;

CaseStmt 	: T_Case T_IntConstant T_Colon StmtList {$$=new CaseStmt(new IntConstant(@2, $2), $4);}
			;

DefaultStmt: T_Default T_Colon StmtList {$$=new DefaultStmt($3);}
;
					
/*Expr ::= LValue = Expr | Constant | LValue | this | Call | ( Expr ) |
Expr + Expr | Expr - Expr | Expr * Expr | Expr / Expr |
Expr % Expr | - Expr | Expr < Expr | Expr <= Expr |
Expr > Expr | Expr >= Expr | Expr == Expr | Expr ! = Expr |
Expr && Expr | Expr || Expr | ! Expr | ReadInteger ( ) |
ReadLine ( ) | New ( ident ) | NewArray ( Expr , Type )*/

Expr	: LValue '=' Expr 	{$$=new AssignExpr($1, new Operator(@2,"="), $3);}
		| Constant 			{$$=$1;}
		| LValue 			{$$=$1;}
		| T_This 			{$$=new This(@1);}
		| Call 				{$$=$1;}
		| '(' Expr ')'		{$$=$2;}
		| Expr '+' Expr 	{$$=new ArithmeticExpr($1, new Operator(@2, "+"), $3);}
		| Expr '-' Expr 	{$$=new ArithmeticExpr($1, new Operator(@2, "-"), $3);}
		| Expr '*' Expr 	{$$=new ArithmeticExpr($1, new Operator(@2, "*"), $3);}
		| Expr '/' Expr 	{$$=new ArithmeticExpr($1, new Operator(@2, "/"), $3);}
		| Expr '%' Expr 	{$$=new ArithmeticExpr($1, new Operator(@2, "%"), $3);}
		| '-' Expr  %prec unaryMinus		{$$=new ArithmeticExpr(new Operator(@1, "-"), $2);}
		| Expr '<' Expr 					{$$=new RelationalExpr($1, new Operator(@2, "<"), $3);}
		| Expr T_LessEqual Expr 			{$$=new RelationalExpr($1, new Operator(@2, "<="), $3);}
		| Expr '>' Expr 					{$$=new RelationalExpr($1, new Operator(@2, ">"), $3);}
		| Expr T_GreaterEqual Expr 			{$$=new RelationalExpr($1, new Operator(@2, ">="), $3);} 
		| Expr T_Equal Expr 				{$$=new EqualityExpr($1, new Operator(@2, "=="), $3);}
		| Expr T_NotEqual Expr 				{$$=new EqualityExpr($1, new Operator(@2, "!="), $3);}
		| Expr T_And Expr 					{$$=new LogicalExpr($1, new Operator(@2, "&&"), $3);}
		| Expr T_Or Expr 					{$$=new LogicalExpr($1, new Operator(@2, "||"), $3);}
		| '!' Expr 							{$$=new LogicalExpr(new Operator(@1, "!"), $2);}
		| T_ReadInteger '(' ')'				{$$=new ReadIntegerExpr(Join(@1,@3));}
		| T_ReadLine '(' ')' 				{$$=new ReadLineExpr(Join(@1,@1));}
		| T_New '(' T_Identifier ')' 		{$$=new NewExpr(Join(@1,@4), new NamedType(new Identifier(@3,$3)));}
		| T_NewArray '(' Expr ',' Type ')'  {$$=new NewArrayExpr(Join(@1,@6), $3, $5);}
		| Expr T_Increment					{$$=new PostfixExpr(new Operator(@2, "++"), $1);}
		| Expr T_Decrement					{$$=new PostfixExpr(new Operator(@2, "--"), $1);}
		;
                      



/*LValue ::= ident | Expr . ident | Expr [ Expr ]*/
LValue	: T_Identifier {$$=new FieldAccess(NULL, new Identifier(@1, $1));}
		| Expr '.' T_Identifier {$$=new FieldAccess($1, new Identifier(@3, $3));} 
		| Expr '[' Expr ']' {$$=new ArrayAccess(Join(@1, @4), $1, $3);}
		;

/*Call ::= ident ( Actuals ) | Expr . ident ( Actuals )*/		
Call	: T_Identifier '(' Actuals ')' {$$=new Call(Join(@1,@4), NULL, new Identifier(@1,$1), $3);}
		| Expr '.' T_Identifier '(' Actuals ')' {$$=new Call(Join(@1,@6), $1, new Identifier(@3,$3), $5);}
		;
			
/*Actuals ::= Expr+, | Episolon*/
Actuals	: NonEmptyExprList {$$=$1;}
		| /* empty */ {$$=new List<Expr*>;}
		;

/*Constant ::= intConstant | doubleConstant | boolConstant | stringConstant | null*/
Constant	: T_IntConstant {$$=new IntConstant(@1,$1);}
			| T_DoubleConstant {$$=new DoubleConstant(@1,$1);}
			| T_BoolConstant {$$=new BoolConstant(@1,$1);}
			| T_StringConstant {$$=new StringConstant(@1,$1);}
			| T_Null {$$=new NullConstant(@1);}
			;


		  
%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/* Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = false;
}
