/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    /* Maximal number of symbol entries */
    #define MAX_SCOPE 25
    #define MAX_SYMBOL 100

    /* Useful tools */
    #define MAX(a, b) ((a) > (b) ? (a) : (b))
    #define MIN(a, b) ((a) < (b) ? (a) : (b))

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror(char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    typedef enum {
	_INT, _FLOAT, _BOOL, _STRING, _UNDIFINED = -1
    } type_t;

    typedef enum {
	_FUNC, _VAR, _LIT
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
    static symbol_t *lookup_symbol(char *name);
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
    int st_type;
}

/* Token without return */
%token PRINT WHILE FOR IF ELSE
%token SEMICOLON COMMA ASSIGN

%token INT FLOAT BOOL STRING VOID

%token LPAREN RPAREN LBRACE RBRACE
%token LBRACK RBRACK

%token TRUE FALSE

%token BREAK CONTINUE RETURN

%right AND OR
%right NOT
%left GTR LSS GEQ LEQ EQL NEQ

%left ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%left INC DEC
%left ADD SUB
%left MUL QUO REM

/* Token with return, which need to sepcify type */
%token <st_type> INT_LIT
%token <st_type> FLOAT_LIT
%token <st_type> STRING_LIT

%token <s_val> IDENT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> Comparator
%type <st_type> Type
%type <st_type> DeclarationStmt

%type <st_type> Literal
%type <st_type> Value
%type <st_type> ExpressionStmt 
%type <st_type> ArithmeticStmt

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
    : INT { $$ = _INT; }
    | FLOAT { $$ = _FLOAT; }
    | STRING { $$ = _STRING; }
    | BOOL { $$ = _BOOL; }
;

Literal
    : INT_LIT {
        printf("INT_LIT %d\n", $<i_val>$);
	$$ = _INT;
    }
    | FLOAT_LIT {
        printf("FLOAT_LIT %f\n", $<f_val>$);
	$$ = _FLOAT;
    }
    | STRING_LIT {
        printf("STRING_LIT %s\n", $<s_val>$);
	$$ = _STRING;
    }
    | TRUE {
        printf("TRUE\n");
	$$ = _BOOL;
    }
    | FALSE {
        printf("FALSE\n");
	$$ = _BOOL;
    }
;

Statement
    : DeclarationStmt SEMICOLON
    | ExpressionStmt SEMICOLON
    | Block
    | IfStmt
    | LoopStmt
    | PrintStmt
;

DeclarationStmt
    : Type IDENT {
	if (!lookup_symbol($2)) {
	    // printf("find declaration id = %s, type = %s\n", $2, $1);
	    insert_symbol($2, _VAR, $1, _UNDIFINED, yylineno);
	} else {
	    // TODO: raise syntax error
	}
	$$ = $1;
    }
    | Type IDENT ASSIGN ExpressionStmt {
	if ($1 != $4) {
	    // TODO: type conflict
	} else if (!lookup_symbol($2)) {
	    // TODO: duplicated var 
	} else {
	    insert_symbol($2, _VAR, $1, _UNDIFINED, yylineno);
	}
	$$ = $1;
    }
    | DeclarationStmt COMMA IDENT {
	if (!lookup_symbol($3)) {
	    // printf("find declaration id = %s, type = %s\n", $2, $1);
	    insert_symbol($3, _VAR, $1, _UNDIFINED, yylineno);
	} else {
	    // TODO: raise syntax error
	}
	$$ = $1;
    }
;

ExpressionStmt
    : ArithmeticStmt
;

ArithmeticStmt
    : LPAREN ArithmeticStmt RPAREN { $$ = $2; }
    | NOT ArithmeticStmt { 
	printf("NOT\n");
	$$ = _BOOL;
    }
    | ArithmeticStmt AND ArithmeticStmt {
	printf("AND\n");
	$$ = _BOOL;
    }
    | ArithmeticStmt OR ArithmeticStmt { 
	printf("OR\n");
	$$ = _BOOL;
    }
    | ArithmeticStmt ADD ArithmeticStmt { 
	printf("ADD\n");
	$$ = MAX($1, $3);
    }
    | ArithmeticStmt SUB ArithmeticStmt { 
	printf("SUB\n");
	$$ = MAX($1, $3);
    }
    | ArithmeticStmt MUL ArithmeticStmt { 
	printf("MUL\n");
	$$ = MAX($1, $3);
    }
    | ArithmeticStmt QUO ArithmeticStmt { 
	printf("QUO\n");
	$$ = MAX($1, $3);
    }
    | ArithmeticStmt REM ArithmeticStmt { 
	printf("REM\n");
	$$ = MAX($1, $3);
    }
    | INC ArithmeticStmt { 
	printf("INC\n");
	$$ = $2;
    }
    | DEC ArithmeticStmt { 
	printf("DEC\n");
	$$ = $2;
    }
    | ArithmeticStmt INC { 
	printf("INC\n");
	$$ = $1;
    }
    | ArithmeticStmt DEC { 
	printf("DEC\n");
	$$ = $1;
    }
    | ArithmeticStmt Comparator ArithmeticStmt %prec ADD {
	printf("%s\n", $2);
	$$ = _BOOL;
    }
    | ADD ArithmeticStmt %prec MUL { 
	printf("POS\n");
	$$ = $2;
    }
    | SUB ArithmeticStmt %prec MUL { 
	printf("NEG\n");
	$$ = $2;
    }
    | Value
;

Value
    : IDENT {
	symbol_t *curr = lookup_symbol($1);
	// printf("address of %s = %d\n", $1, addr);
	if (curr) 
	    printf("IDENT (name=%s, address=%d)\n", $1, curr->address);
	else 
	    printf("error:%d: undefined: %s\n", yylineno,$1);
	$$ = curr->type;
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
    : GTR { $$ = "GTR"; }
    | LSS { $$ = "LSS"; }
    | GEQ { $$ = "GEQ"; }
    | LEQ { $$ = "LEQ"; }
    | EQL { $$ = "EQL"; }
    | NEQ { $$ = "NEQ"; }
;

PrintStmt
    : PRINT LPAREN ExpressionStmt RPAREN SEMICOLON { 
	printf("PRINT %s\n", get_type_name($3));
    }
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

static symbol_t *lookup_symbol(char *name) 
{
    for (int i = 0; i <= curr_scope; i++) {
	for (int j = 0; j < symbol_num[curr_scope]; j++) {
	    if (!strcmp(symbol_table[i][j].name, name)) 
		return &symbol_table[i][j];
	}
    }
    return NULL;
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
