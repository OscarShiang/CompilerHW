/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    static int print_label_num = 0;
    static int cmp_label_num = 0;

    #define labelgen(fmt, num) fprintf(fout, fmt "_%d:\n", num)

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;
    
    /* Maximal number of symbol entries */
    #define MAX_SCOPE 25
    #define MAX_SYMBOL 100

    /* Useful tools */
    #define MAX(a, b) ((a) > (b) ? (a) : (b))
    #define MIN(a, b) ((a) < (b) ? (a) : (b))

    #define SEMANTIC_CHECK(cond, fmt, ...)                    \
        do {                                                  \
            if (cond) {                                       \
                printf("error:%d: " fmt "\n", ##__VA_ARGS__); \
                HAS_ERROR = true;                             \
            }                                                 \
        } while (0)

    #define codegen(fmt, ...)                       \
        do {                                        \
            for (int i = 0; i < INDENT; i++) {      \
                fprintf(fout, "\t");                \
            }                                       \
            fprintf(fout, fmt "\n", ##__VA_ARGS__); \
        } while (0)

    #define codegen_type(type, instr, ...)         \
        do {                                       \
            switch (type) {                        \
            case _INT:                             \
            case _BOOL:                            \
                codegen("i" instr, ##__VA_ARGS__); \
                break;                             \
            case _FLOAT:                           \
                codegen("f" instr, ##__VA_ARGS__); \
                break;                             \
            case _STRING:                          \
            case _ARRAY:                           \
                codegen("a" instr, ##__VA_ARGS__); \
                break;                             \
            default:                               \
                (void) 0;                          \
            }                                      \
        } while (0)

    #define codegen_cmp(type, instr)                         \
        do {                                                 \
            if (type == _INT)                                \
                codegen_type(type, "sub");                   \
            else                                             \
                codegen("fcmpl");                            \
            codegen("%s CMP_TRUE_%d", instr, cmp_label_num); \
            codegen("iconst_0");                             \
            codegen("goto CMP_END_%d", cmp_label_num);       \
            labelgen("CMP_TRUE", cmp_label_num);             \
            codegen("iconst_1");                             \
            labelgen("CMP_END", cmp_label_num++);            \
        } while (0)

    /* Other global variables */
    FILE *fout = NULL;
    bool HAS_ERROR = false;
    int INDENT = 0;
    
    void yyerror (char const *s)
    {
        HAS_ERROR = true;
        printf("error:%d: %s\n", yylineno, s);
    }

    typedef enum {
	    _ARRAY, _BOOL, _INT, _FLOAT, _STRING, _UNDEFINED = -1
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
        int scope;
    } symbol_t;

    int curr_scope = 0;
    static int address = 0;

    static symbol_t symbol_table[MAX_SCOPE][MAX_SYMBOL];
    static int symbol_num[MAX_SCOPE];

    static char *get_type_name(type_t type);

    /* Symbol table function - you can add new function if needed. */
    static void create_symbol();
    static symbol_t *insert_symbol(char *name, kind_t kind, type_t type, type_t eletype, int lineno);
    static symbol_t *lookup_symbol(char *name);
    static void dump_symbol(int level);

    static char *get_assignop_name(char *op);

    #define declare_label_stack(name)                                   \
        static int name##_label_num = 0;                                \
        static int name##_stack_idx = -1;                               \
        static int name##_label_stack[MAX_SCOPE];                       \
        static inline void push_##name##_label(int label)               \
        {                                                               \
            name##_label_stack[++name##_stack_idx] = label;             \
        }                                                               \
        static inline void pop_##name##_label() { --name##_stack_idx; } \
        static inline int get_##name##_label()                          \
        {                                                               \
            return name##_label_stack[name##_stack_idx];                \
        }

    declare_label_stack(loop);
    declare_label_stack(if);
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
%type <s_val> CompoundOps
%type <s_val> IncAndDec

%type <st_type> Type
%type <st_type> DeclarationStmt

%type <st_type> Literal
%type <st_type> Value
%type <st_type> Variable
%type <st_type> ExpressionStmt 
%type <st_type> ArithmeticStmt
%type <st_type> AssignmentStmt
%type <st_type> CompoundStmt

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
	    codegen("ldc %d", $<i_val>$);
        $$ = _INT;
    }
    | FLOAT_LIT {
        codegen("ldc %f", $<f_val>$);
	    $$ = _FLOAT;
    }
    | STRING_LIT {
        codegen("ldc \"%s\"", $<s_val>$);
	    $$ = _STRING;
    }
    | TRUE {
        codegen("iconst_1");
	    $$ = _BOOL;
    }
    | FALSE {
        codegen("iconst_0");
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
        symbol_t *curr = lookup_symbol($2);
        if (!curr || curr->scope != curr_scope) {
            curr = insert_symbol($2, _VAR, $1, _UNDEFINED, yylineno);
        } else {
            SEMANTIC_CHECK(true,
                            "%s redeclared in this block. previous declaration at line %d",
                            yylineno, $2, curr->lineno);
        }
        $$ = $1;

        codegen("ldc %s", ($1 == _STRING ? "\"\"" : $1 == _INT ? "0" : "0.0"));
        codegen_type($1, "store %d", curr->address);
    }
    | Type IDENT LBRACK Literal RBRACK {
        symbol_t *curr = lookup_symbol($2);
        if (!curr || curr->scope != curr_scope) {
            curr = insert_symbol($2, _VAR, _ARRAY, $1, yylineno);
        } else {
            // TODO: raise syntax error
        }
        $$ = $1;

        codegen("newarray %s", get_type_name($1));
        codegen_type(_ARRAY, "store %d", curr->address);
    }
    | Type IDENT ASSIGN ExpressionStmt {
        symbol_t *curr = lookup_symbol($2);
        if ($1 != $4) {
            // TODO: type conflict
        } else if (curr && curr->scope == curr_scope) {
            // TODO: duplicated var 
        } else {
            curr = insert_symbol($2, _VAR, $1, _UNDEFINED, yylineno);
        }
        $$ = $1;

        codegen_type($1, "store %d", curr->address);
    }
    | DeclarationStmt COMMA IDENT {
        symbol_t *curr = lookup_symbol($3);
        if (!curr || curr->scope != curr_scope) {
            curr = insert_symbol($3, _VAR, $1, _UNDEFINED, yylineno);
        } else {
            // TODO: raise syntax error
        }
        $$ = $1;

        codegen("ldc %s", ($1 == _STRING ? "\"\"" : $1 == _INT ? "0" : "0.0"));
        codegen_type($1, "store %d", curr->address);
    }
    | DeclarationStmt COMMA IDENT LBRACK Literal RBRACK {
        symbol_t *curr = lookup_symbol($3);
        if (!curr || curr->scope != curr_scope) {
            curr = insert_symbol($3, _VAR, _ARRAY, $1, yylineno);
        } else {
            // TODO: raise syntax error
        }
        $$ = $1;

        codegen("newarray %s", get_type_name($1));
        codegen_type(_ARRAY, "store %d", curr->address);
    }
;

ExpressionStmt
    : ArithmeticStmt
    | AssignmentStmt
    | CompoundStmt
;

ArithmeticStmt
    : LPAREN ArithmeticStmt RPAREN { $$ = $2; }
    | NOT ArithmeticStmt { 
        codegen("iconst_1");
        codegen("ixor");
        $$ = _BOOL;
    }
    | ArithmeticStmt AND ArithmeticStmt {
        SEMANTIC_CHECK($1 != _BOOL || $3 != _BOOL,
                 "invalid operation: (operator AND not defined on %s)",
                 yylineno, get_type_name(MAX($1, $3)));
        codegen("iand");
        $$ = _BOOL;
    }
    | ArithmeticStmt OR ArithmeticStmt { 
        SEMANTIC_CHECK($1 != _BOOL || $3 != _BOOL,
                 "invalid operation: (operator OR not defined on %s)",
                 yylineno, get_type_name(MAX($1, $3)));
        codegen("ior");
        $$ = _BOOL;
    }
    | ArithmeticStmt ADD ArithmeticStmt { 
        SEMANTIC_CHECK($1 != $3,
                 "invalid operation: ADD (mismatched types %s and %s)",
                 yylineno, get_type_name($1), get_type_name($3));
        codegen_type($1, "add");
        $$ = $1;
    }
    | ArithmeticStmt SUB ArithmeticStmt {
        SEMANTIC_CHECK($1 != $3,
                 "invalid operation: SUB (mismatched types %s and %s)",
                 yylineno, get_type_name($1), get_type_name($3));
        codegen_type($1, "sub");
        $$ = $1;
    }
    | ArithmeticStmt MUL ArithmeticStmt {
        SEMANTIC_CHECK($1 != $3,
                 "invalid operation: MUL (mismatched types %s and %s)",
                 yylineno, get_type_name($1), get_type_name($3));
        codegen_type($1, "mul");
        $$ = $1;
    }
    | ArithmeticStmt QUO ArithmeticStmt {
        SEMANTIC_CHECK($1 != $3,
                 "invalid operation: QUO (mismatched types %s and %s)",
                 yylineno, get_type_name($1), get_type_name($3));
        codegen_type($1, "div");
        $$ = $1;
    }
    | ArithmeticStmt REM ArithmeticStmt { 
        SEMANTIC_CHECK($1 != _INT || $3 != _INT,
                 "invalid operation: (operator REM not defined on %s)",
                 yylineno, get_type_name(MAX($1, $3)));
        codegen("irem");
        $$ = $1;
    }
    | IncAndDec IDENT {
        symbol_t *curr = lookup_symbol($2);
        if (curr && curr->scope <= curr_scope) {
            printf("IDENT (name=%s, address=%d)\n", $2, curr->address);
            type_t type = MAX(curr->type, curr->eletype);
            codegen_type(type, "const_1");
            codegen_type(type, "load %d", curr->address);
            codegen_type(type, "%s", $2);
            codegen_type(type, "store %d", curr->address);
            $$ = type;
        } else {
            printf("error:%d: undefined: %s\n", yylineno, $2);
            $$ = _UNDEFINED;
        }
    }
    | IDENT IncAndDec { 
        symbol_t *curr = lookup_symbol($1);
        if (curr && curr->scope <= curr_scope) {
            type_t type = MAX(curr->type, curr->eletype);
            codegen_type(type, "load %d", curr->address);
            codegen_type(type, "const_1");
            codegen_type(type, "%s", $2);
            codegen_type(type, "store %d", curr->address);
            $$ = type;
        } else {
            printf("error:%d: undefined: %s\n", yylineno,$1);
            $$ = _UNDEFINED;
        }
    }
    | ArithmeticStmt Comparator ArithmeticStmt %prec ADD {
        codegen_cmp($1, $2);
        $$ = _BOOL;
    }
    | ADD ArithmeticStmt %prec MUL { $$ = $2; }
    | SUB ArithmeticStmt %prec MUL { 
        codegen_type($2, "neg");
        $$ = $2;
    }
    | Value
;

IncAndDec
    : INC { $$ = "add"; }
    | DEC { $$ = "sub"; }
;

AssignmentStmt
    : IDENT ASSIGN ArithmeticStmt {
        symbol_t *curr = lookup_symbol($1);
        if (curr && curr->scope <= curr_scope) {
            type_t type = MAX(curr->type, curr->eletype);
            SEMANTIC_CHECK((curr->type != _UNDEFINED) && (curr->type != $3),
                 "invalid operation: ASSIGN (mismatched types %s and %s)",
                 yylineno, get_type_name(type), get_type_name($3));
            codegen_type(type, "store %d", curr->address);
            $$ = type;
        } else {
            SEMANTIC_CHECK(true, "undefined: %s", yylineno, $1);
            $$ = _UNDEFINED;
        }
    }
    | Variable LBRACK ArithmeticStmt RBRACK ASSIGN ArithmeticStmt { 
        SEMANTIC_CHECK(($1 != _UNDEFINED) && ($1 != $6),
                 "invalid operation: ASSIGN (mismatched types %s and %s)",
                 yylineno, get_type_name($1), get_type_name($6));
        codegen_type($1, "astore");
    }
    | Literal ASSIGN ArithmeticStmt {
        SEMANTIC_CHECK(true, "cannot assign to %s", yylineno, get_type_name($1));
    }
    
;

CompoundStmt
    : IDENT CompoundOps ArithmeticStmt {
        symbol_t *curr = lookup_symbol($1);
        if (curr && curr->scope <= curr_scope) {
            SEMANTIC_CHECK((curr->type != _UNDEFINED) && (curr->type != $3),
                 "invalid operation: %s (mismatched types %s and %s)",
                 yylineno, get_assignop_name($2), get_type_name(curr->type), get_type_name($3));
            codegen_type(curr->type, "load %d", curr->address);
            codegen("swap");
            codegen_type(curr->type, "%s", $2);
            codegen_type(curr->type, "store %d", curr->address);
            $$ = curr->type;
        } else {
            SEMANTIC_CHECK(true, "undefined: %s", yylineno, $1);
            $$ = _UNDEFINED;
        }
    }
    | Literal CompoundOps ArithmeticStmt {
        SEMANTIC_CHECK(true, "cannot assign to %s", yylineno, get_type_name($1));
    }
;

CompoundOps
    : ADD_ASSIGN { $$ = "add"; }
    | SUB_ASSIGN { $$ = "sub"; }
    | MUL_ASSIGN { $$ = "mul"; }
    | QUO_ASSIGN { $$ = "div"; }
    | REM_ASSIGN { $$ = "rem"; }
;

Value
    : Variable 
    | Variable LBRACK ArithmeticStmt RBRACK { codegen_type($1, "aload"); }
    | Literal
    | LPAREN Type RPAREN Value {
        codegen("%c2%c", get_type_name($4)[0], get_type_name($2)[0]);
        $$ = $2;
    }
;

Variable
    : IDENT {
        symbol_t *curr = lookup_symbol($1);
        if (curr && curr->scope <= curr_scope) {
            codegen_type(curr->type, "load %d", curr->address);
            $$ = MAX(curr->type, curr->eletype);
        } else {
            SEMANTIC_CHECK(true, "undefined: %s", yylineno, $1);
            $$ = _UNDEFINED;
        }
    }
;

Block
    : LBRACE StatementList RBRACE {
        symbol_num[curr_scope + 1] = 0;
    }
;

IfStmt
    : IF LPAREN Condition RPAREN IfBlock ElseStmt {
        labelgen("IF_END", get_if_label());
        pop_if_label();
    }
;

ElseStmt
    : ELSE IfStmt
    | ELSE Block
    |
;

IfBlock:
    Block {
        codegen("goto IF_END_%d", get_if_label());
        labelgen("IF_ELSE", get_if_label());
    }
;

Condition
    : ArithmeticStmt {
        push_if_label(if_label_num++);
        codegen("ifeq IF_ELSE_%d", get_if_label());
    }
;

LoopStmt
    : While LPAREN LoopCondition RPAREN Block {
        codegen("goto LOOP_BEGIN_%d", get_loop_label());
        labelgen("LOOP_END", get_loop_label());
        pop_loop_label();
    }
    | While LPAREN LoopCondition RPAREN SEMICOLON {
        codegen("goto LOOP_BEGIN_%d", get_loop_label());
        labelgen("LOOP_END", get_loop_label());
        pop_loop_label();
    }
    | FOR LPAREN ForClause RPAREN Block {
        codegen("goto LOOP_POST_%d", get_loop_label());
        labelgen("LOOP_END", get_loop_label());
        pop_loop_label();
    }
    | FOR LPAREN ForClause RPAREN SEMICOLON {
        codegen("goto LOOP_POST_%d", get_loop_label());
        labelgen("LOOP_END", get_loop_label());
        pop_loop_label();
    }
;

While 
    : WHILE {
        push_loop_label(loop_label_num++);
        labelgen("LOOP_BEGIN", get_loop_label());
    }
;

LoopCondition
    : ArithmeticStmt {
        SEMANTIC_CHECK($1 != _BOOL,
                 "non-bool (type %s) used as for condition",
                 yylineno + 1, get_type_name($1));

        codegen("ifeq LOOP_END_%d", get_loop_label());
    }
;

ForClause
    : InitStmt SEMICOLON ForCondition SEMICOLON PostStmt
;

InitStmt
    : Type IDENT ASSIGN ArithmeticStmt {
        curr_scope++;
        symbol_t *curr = lookup_symbol($2);
        if ($1 != $4) {
            // TODO: type conflict
        } else if (curr && curr->scope == curr_scope) {
            // TODO: duplicated var 
        } else {
            insert_symbol($2, _VAR, $1, _UNDEFINED, yylineno);
        }
        curr_scope--;
    }
    | IDENT ASSIGN ArithmeticStmt {
        symbol_t *curr = lookup_symbol($1);
        if (curr && curr->scope <= curr_scope) {
            type_t type = MAX(curr->type, curr->eletype);
            SEMANTIC_CHECK((curr->type != _UNDEFINED) && (curr->type != $3),
                 "invalid operation: ASSIGN (mismatched types %s and %s)",
                 yylineno, get_type_name(type), get_type_name($3));
            codegen_type(type, "store %d", curr->address);
            push_loop_label(loop_label_num++);
            labelgen("LOOP_BEGIN", get_loop_label());
        } else {
            SEMANTIC_CHECK(true, "undefined: %s", yylineno, $1);
        }
    }
;

ForCondition
    : LoopCondition {
        codegen("goto LOOP_BODY_%d", get_loop_label());
        labelgen("LOOP_POST", get_loop_label());
    }
;

PostStmt
    : ArithmeticStmt {
        codegen("goto LOOP_BEGIN_%d", get_loop_label());
        labelgen("LOOP_BODY", get_loop_label());
    }
;

Comparator
    : GTR { $$ = "ifgt"; }
    | LSS { $$ = "iflt"; }
    | GEQ { $$ = "ifge"; }
    | LEQ { $$ = "ifle"; }
    | EQL { $$ = "ifeq"; }
    | NEQ { $$ = "ifne"; }
;

PrintStmt
    : PRINT LPAREN ExpressionStmt RPAREN SEMICOLON {
        if ($3 == _BOOL) {
            codegen("ifne PRINT_BOOL_TRUE_%d", print_label_num);
            codegen("ldc \"false\"");
            codegen("goto PRINT_BEGIN_%d", print_label_num);
            labelgen("PRINT_BOOL_TRUE", print_label_num);
            codegen("ldc \"true\"");
            labelgen("PRINT_BEGIN", print_label_num++);
        }

        codegen("getstatic java/lang/System/out Ljava/io/PrintStream;");
        codegen("swap");
        switch ($3) {
        case _INT:
            codegen("invokevirtual java/io/PrintStream/print(I)V");
            break;
        case _FLOAT:
            codegen("invokevirtual java/io/PrintStream/print(F)V");
            break;
        case _BOOL:
        case _STRING:
            codegen("invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V");
            break;
        }
    }
;

%%

static void create_symbol() 
{
    for (int i = 0; i < MAX_SCOPE; i++) {
        symbol_num[i] = 0;
        for (int j = 0; j < MAX_SYMBOL; j++) {
            symbol_table[i][j].name = NULL;
            symbol_table[i][j].type = _UNDEFINED;
            symbol_table[i][j].eletype = _UNDEFINED;
            symbol_table[i][j].lineno = -1;
            symbol_table[i][j].scope = -1;
        }
    }
}

static symbol_t *insert_symbol(char *name, kind_t kind, type_t type, type_t eletype, int lineno)
{
    symbol_t new_symbol = {
        .name = name,
        .kind = kind,
        .type = type,
        .eletype = eletype,
        .lineno = lineno,
        .scope = curr_scope,
        .address = address++,
    };
    symbol_table[curr_scope][symbol_num[curr_scope]] = new_symbol;
    return &symbol_table[curr_scope][symbol_num[curr_scope]++];
}

static symbol_t *lookup_symbol(char *name) 
{
    for (int i = curr_scope; i >= 0; i--) {
        for (int j = 0; j < symbol_num[i]; j++) {
            if (!strcmp(symbol_table[i][j].name, name)) 
                return &symbol_table[i][j];
        }
    }
    return NULL;
}

static void dump_symbol(int level) 
{
    printf("> Dump symbol table (scope level: %d)\n", level);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n", "Index", "Name", "Type", "Address", "Lineno",
            "Element type");
    for (int i = 0; i < symbol_num[level]; i++) {
        symbol_t *curr = &symbol_table[level][i];
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
    case _ARRAY:
        return "array";
    default:
        return "-";
    }
}

static char *get_assignop_name(char *op)
{
    if (!strcmp(op, "add"))
        return "ADD_ASSGIGN";
    else if (!strcmp(op, "sub"))
        return "SUB_ASSGIGN";
    else if (!strcmp(op, "mul"))
        return "MUL_ASSGIGN";
    else if (!strcmp(op, "div"))
        return "QUO_ASSGIGN";
    else if (!strcmp(op, "rem"))
        return "REM_ASSGIGN";
    else 
        return "";
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

    /* Codegen output init */
    char *bytecode_filename = "hw3.j";
    fout = fopen(bytecode_filename, "w");
    codegen(".source hw3.j");
    codegen(".class public Main");
    codegen(".super java/lang/Object");
    codegen(".method public static main([Ljava/lang/String;)V");
    codegen(".limit stack 100");
    codegen(".limit locals 100");
    INDENT++;

    yyparse();

    printf("Total lines: %d\n", yylineno);

    /* Codegen end */
    codegen("return");
    INDENT--;
    codegen(".end method");
    fclose(fout);
    fclose(yyin);

    if (HAS_ERROR) {
        remove(bytecode_filename);
    }
    return 0;
}