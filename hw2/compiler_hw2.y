/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    #define MAX_SCOPE 25
    #define MAX_SYMBOL 100

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror(char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    typedef enum {
	_INT, _FLOAT, _STRING, _BOOL, _UNDIFINED = -1
    } type_t;

    typedef enum {
	_FUNC, _VAR
    } kind_t;

    typedef struct {
	char *name;
	kind_t kind;
	type_t type;
	type_t eletype;
	int lineno;
	int address;
    } symbol_t;

    static int curr_scope = 0;
    static int address = 0;

    static symbol_t symbol_table[MAX_SCOPE][MAX_SYMBOL];
    static int symbol_num[MAX_SCOPE];

    static char *get_type_name(type_t type);

    /* Symbol table function - you can add new function if needed. */
    static void create_symbol();
    static void insert_symbol(char *name, kind_t kind, type_t type, type_t eletype, int lineno);
    static int lookup_symbol(char *name);
    static void dump_symbol();
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    /* ... */
}

/* Token without return */
%token PRINT WHILE FOR IF ELSE
%token SEMICOLON COMMA ASSIGN

%token INT FLOAT BOOL STRING VOID

%token AND OR NOT
%token ADD SUB MUL QUO REM INC DEC
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token GTR LSS GEQ LEQ EQL NEQ

%token LPAREN RPAREN LBRACE RBRACE
%token LBRACK RBRACK

%token TRUE FALSE

%token BREAK CONTINUE RETURN

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT

%token <s_val> IDENT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> TypeName 
%type <s_val> DeclarationStmt

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList
;

StatementList
    : StatementList Statement
    | Statement
;

Type
    : TypeName
;

TypeName
    : INT { $$ = _INT; }
    | FLOAT { $$ = _FLOAT; }
    | STRING { $$ = _STRING; }
    | BOOL { $$ = _BOOL; }
;

Literal
    : INT_LIT {
        printf("INT_LIT %d\n", $<i_val>$);
    }
    | FLOAT_LIT {
        printf("FLOAT_LIT %f\n", $<f_val>$);
    }
    | STRING_LIT {
        printf("STRING_LIT %s\n", $<s_val>$);
    }
    | TRUE {
        printf("BOOL_LIT %d\n", $<i_val>$);
    }
    | FALSE {
        printf("BOOL_LIT %d\n", $<i_val>$);
    }
;

Statement
    : DeclarationStmt SEMICOLON
    | ExpressionStmt
    | Block
    | IfStmt
    | LoopStmt
    | PrintStmt
;

DeclarationStmt
    : TypeName IDENT {
	if (lookup_symbol($2) == -1) {
	    // printf("find declaration id = %s, type = %s\n", $2, $1);
	    insert_symbol($2, _VAR, $1, _UNDIFINED, yylineno);
	} else {
	    // TODO: raise syntax error
	}
    }
    | TypeName IDENT ASSIGN ExpressionStmt
    | DeclarationStmt COMMA IDENT
;

ExpressionStmt
    : ArithmeticStmt SEMICOLON
;

ArithmeticStmt
    : ArithmeticStmt ADD ArithmeticStmt { printf("ADD\n"); }
    | ArithmeticStmt SUB ArithmeticStmt { printf("SUB\n"); }
    | ArithmeticStmt MUL ArithmeticStmt { printf("MUL\n"); }
    | ArithmeticStmt QUO ArithmeticStmt { printf("QUO\n"); }
    | ArithmeticStmt REM ArithmeticStmt { printf("REM\n"); }
    | INC ArithmeticStmt { printf("INC\n"); }
    | DEC ArithmeticStmt { printf("DEC\n"); }
    | ArithmeticStmt INC { printf("INC\n"); }
    | ArithmeticStmt DEC { printf("DEC\n"); }
    | Value
;

Value
    : IDENT {
	int addr = lookup_symbol($1);
	// printf("address of %s = %d\n", $1, addr);
	if (addr != -1) 
	    printf("IDENT (name=%s, address=%d)\n", $1, addr);
	else 
	    printf("error:%d: undefined: %s\n", yylineno, $1);
    }
    | Literal
;

Block
    : LBRACE StatementList RBRACE
;

IfStmt
    : IF LPAREN Condition RPAREN Block
    | IF LPAREN Condition RPAREN ElseStmt
;

ElseStmt
    : ELSE IfStmt
    | ELSE Block
;

LoopStmt
    : WHILE LPAREN Condition RPAREN Block
    | WHILE LPAREN Condition RPAREN SEMICOLON
    | FOR LPAREN Condition RPAREN Block
    | FOR LPAREN Condition RPAREN SEMICOLON
;

Condition
    : ArithmeticStmt Comparator ArithmeticStmt
    | ArithmeticStmt
;

Comparator
    : GTR
    | LSS
    | GEQ
    | LEQ
    | EQL
    | NEQ
;

PrintStmt
    : PRINT LPAREN STRING_LIT RPAREN SEMICOLON
    | PRINT LPAREN STRING_LIT ArgList RPAREN SEMICOLON
;

ArgList
    : Value COMMA ArgList
    | Value
;

%%

static void create_symbol() 
{
    for (int i = 0; i < MAX_SCOPE; i++) {
	symbol_num[i] = 0;
	for (int j = 0; j < MAX_SYMBOL; j++) {
	    symbol_table[i][j].name = NULL;
	    symbol_table[i][j].type = _UNDIFINED;
	    symbol_table[i][j].eletype = _UNDIFINED;
	    symbol_table[i][j].lineno = -1;
	}
    }
}

static void insert_symbol(char *name, kind_t kind, type_t type, type_t eletype, int lineno)
{
    symbol_t new_symbol = {
	.name = name,
	.kind = kind,
	.type = type,
	.eletype = eletype,
	.lineno = lineno,
	.address = address++,
    };
    symbol_table[curr_scope][symbol_num[curr_scope]++] = new_symbol;
    printf("> Insert {%s} into symbol table (scope level: %d)\n", name, curr_scope);
}

static int lookup_symbol(char *name) 
{
    for (int i = 0; i <= curr_scope; i++) {
	for (int j = 0; j < symbol_num[curr_scope]; j++) {
	    if (!strcmp(symbol_table[i][j].name, name)) 
		return symbol_table[i][j].address;
	}
    }
    return -1;
}

static void dump_symbol() 
{
    printf("> Dump symbol table (scope level: %d)\n", curr_scope);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n", "Index", "Name", "Type", "Address", "Lineno",
	"Element type");
    for (int i = 0; i < symbol_num[curr_scope]; i++) {
	symbol_t *curr = &symbol_table[curr_scope][i];
	printf("%-10d%-10s%-10s%-10d%-10d%s\n",
	    i, curr->name,
	    get_type_name(curr->type),
	    curr->address, curr->lineno,
	    get_type_name(curr->eletype));
    }
}

static char *get_type_name(type_t type) 
{
    switch (type) {
    case _INT:
	return "int";
    case _FLOAT:
	return "float";
    case _STRING:
	return "string";
    case _BOOL:
	return "bool";
    default:
	return "-";
    }
}

static type_t get_type_from_name(char *name) 
{
    if (!strcmp(name, "int")) 
	return _INT;
    else if (!strcmp(name, "float"))
	return _FLOAT;
    else if (!strcmp(name, "bool"))
	return _BOOL;
    else if (!strcmp(name, "string"))
	return _STRING;
    return _UNDIFINED;
}

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    create_symbol();

    yyparse();

    dump_symbol();
    printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}
