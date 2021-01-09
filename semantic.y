%{
void yyerror (char *s);
int yylex();
#include <stdio.h>     /* C declarations used in actions */
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

extern char* yytext;
extern FILE* yyin;
extern int yylineno;
int lineError = 0;

struct var {
	int type;

	char id[100];
	double value;
	char valueStr[1000];

	int cnst;
};

struct var* initializeVar();

int totalVar = 0;
struct var variables[100];

struct var* temporaryPointNum(double, int);
struct var* temporaryPointStr(void*, int);
struct var* temporaryPointVar(char*);

void freeVar(struct var* v);
int getVariableIndex(char*);
void updateValue(char*, struct var*);
void FloatingPointException(int);
void pushVariable(char*, int, struct var*);
void pushEmptyVariable(char*, int);
struct var* comp(struct var*, struct var*, int);
void printValue(struct var*);

%}

%union {
	double num; 
	char string[1000]; 
	int type_id; 
	struct var* strct;
}     

%start lines
%token print

%type <type_id> DATA_TYPE
%token <type_id> Integer Float Double Character Bool String
%token Const

%token GEQ LEQ AND OR EQEQ LS GE
%token PLUS MINUS PROD DIV EQUAL

%type<num> stat
%token IF
%type<num> smtm smtm_type smtm_types smtm_fun ELSE_ ELIF_ ELIF_S
%token ELSE
%token ELIF

%token FUN RETURN
%type<num> FUNCTION

%token <string> String_Value Character_Value

%token exit_command
%token <num> number number_r
%token <string> IDENTIFIER
%type <num> line lines

%type <strct> exp term 

%type <num> assignment // TODO verify

%nonassoc IF ELSE ELIF

%right EQUAL

%left EQEQ
%left GEQ LEQ LS GE

%left MINUS PLUS
%left DIV PROD

%left OR
%left AND


%%

/* descriptions of expected inputs     corresponding actions (in C) */



lines   : line			 			{;}
		| lines line				{;}
		;

line 	: assignment ';'				{;}
		| exit_command ';'				{exit(EXIT_SUCCESS);}
		| print exp ';'					{printValue($2);}
		| stat 							{;}
		| FUNCTION 				   		{;}
		;


DATA_TYPE   : Integer   	 {$$ = $1;}
			| Float			 {$$ = $1;}
			| Double		 {$$ = $1;}
			| Character 	 {$$ = $1;}
			| Bool 	 		 {$$ = $1;}
			| String		 {$$ = $1;}
			;

assignment  : DATA_TYPE IDENTIFIER	 					{pushEmptyVariable($2, $1);}
			| DATA_TYPE IDENTIFIER EQUAL exp  			{pushVariable($2, $1, $4);}
		
			| Const DATA_TYPE IDENTIFIER EQUAL exp  		{pushVariable($3, $2, $5);} // TODO const

			| DATA_TYPE IDENTIFIER '[' exp ']'			{pushEmptyVariable($2, $1);} // TODO exp

			| IDENTIFIER EQUAL exp   						{updateValue($1, $3);}
			;


exp    	: term                     	{$$ = $1;}
     	| '(' exp ')'			   	{$$ = $2;}
       	| exp PLUS exp              {$$ = comp($1, $3, PLUS);} // todo if it's IDENTIFIER
       	| exp MINUS exp             {$$ = comp($1, $3, MINUS);}
       	| exp PROD exp              {$$ = comp($1, $3, PROD);}
        | exp DIV exp          	   	{$$ = comp($1, $3, DIV);} // TODO 0 division

		| exp AND exp              	{$$ = comp($1, $3, AND);}
		| exp OR exp               	{$$ = comp($1, $3, OR);}
		| exp LS exp 				{$$ = comp($1, $3, LS) ;}
		| exp GE exp 				{$$ = comp($1, $3, GE);}
		| exp LEQ exp 				{$$ = comp($1, $3, LEQ);}
		| exp GEQ exp 				{$$ = comp($1, $3, GEQ);}
		| exp EQEQ exp 				{$$ = comp($1, $3, EQEQ);}
		;


term	: IDENTIFIER			{$$ = temporaryPointVar($1);} 
   		| number                {$$ = temporaryPointNum($1, Integer);}
		| number_r				{$$ = temporaryPointNum($1, Double);}
		| Character_Value		{$$ = temporaryPointStr($1, Character);}
		| String_Value			{$$ = temporaryPointStr($1, String);}
        ;

stat	: IF '(' exp ')' smtm				{;}
		| IF '(' exp ')' smtm ELIF_S ELSE_ 	{;}
		| IF '(' exp ')' smtm ELSE_ 		{;}
		| IF '(' exp ')' smtm ELIF_S	 	{;}
		;

ELSE_   : ELSE smtm 						{;}
		;

ELIF_S  : ELIF_
		| ELIF_S ELIF_
		;

ELIF_   : ELIF '(' exp ')' smtm				{;}
		;

smtm 	: '{' smtm_types '}'				{;}
		| '{' '}'							{;}
		;

smtm_types  : smtm_type 					{;}
			| smtm_types smtm_type
			;

smtm_type 	: assignment ';'			{;}
			| exp        ';'			{;}
			| print exp ';'				{printValue($2);}
			| stat 						{;}
			;


FUNCTION 	: DATA_TYPE FUN '(' ')' smtm_fun 		{;}
			;

smtm_fun	: '{' smtm_types RETURN exp ';' '}' 		{;}
			| '{' RETURN exp ';' '}'				{;}
			;

%%


struct var* temporaryPointNum(double val, int type) {
	struct var *v = initializeVar();

	v->value = val;
	v->type = type;

	return v;
}

struct var* temporaryPointStr(void* val, int type) {
	struct var *v = initializeVar();

	v->type = type;

	if (type == String) {
		sprintf(v->valueStr, "%s", (char*)val);
	} else {
		v->value = ((char*)val)[0];
	}

	return v;
}

struct var* temporaryPointVar(char* id) {
	int i = getVariableIndex(id);

	if (i == -1) {
		printf("Variable %s was not declared in this scope\n", id);
		exit(0);
	}

	struct var *v = &variables[i];
	struct var *exp = initializeVar();

	exp->type = v->type;

	if (v->type == String) {
		sprintf(exp->valueStr, "%s", v->valueStr);
	} else {
		exp->value = v->value;
	}

	return exp;
}

void freeVar(struct var* v) {
	if (strlen(v->id) == 0) {
		free(v);
	}
}

int getVariableIndex(char* varName) {
	for (int i = 0; i < totalVar; i++) {
		if (strcmp(varName, variables[i].id) == 0) {
			return i;
		}
	}

	return -1;
} 

void updateValue(char* id, struct var* exp) {
	int i = getVariableIndex(id);

	if (i == -1) {
		printf("Variable %s was not declared in this scope\n", id);
		exit(0);
	} 

	struct var *vr = variables + i;

	if (vr->type == String) {
		sprintf(vr->valueStr, "%s", exp->valueStr);
	} else {
		vr->value = exp->value;
	}

}

void FloatingPointException(int val) {
	if(!val) {
		printf("Nu se poate imparti la 0\n");
		exit(0);
	}
}

void pushEmptyVariable(char* id, int type) {
	int i = getVariableIndex(id);

	if (i != -1) {
		printf("The variable %s was already declared here\n", id);
		exit(0);
	}

	struct var *v = variables + totalVar;
	
	sprintf(v->id, "%s", id);
	v->type = type;

	if (type == String) {
		v->value = 0;
	} else {
		sprintf(v->valueStr, "%s", "");
	}

	totalVar++;
}

void pushVariable(char* id, int type, struct var* exp) {
	int i = getVariableIndex(id);

	if (i != -1) {
		printf("The variable %s was already declared here\n", id);
		exit(0);
	}

	struct var *v = variables + totalVar;
	
	sprintf(v->id, "%s", id);
	v->type = type;

	if (type == String) {
		sprintf(v->valueStr, "%s", exp->valueStr);
	} else {
		v->value = exp->value;
	}

	freeVar(exp);
	totalVar++;
}

struct var* comp(struct var* a, struct var* b, int op_type) {
	
	struct var* v = initializeVar();

	switch (op_type) {
	case PLUS:
		v->type = Double;
		v->value = a->value + b->value;
		break;
	case MINUS:
		v->type = Double;
		v->value = a->value - b->value;
		break;
	case PROD:
		if (a->type == Integer && b->type == Integer) {
			v->type = Integer;
		} else {
			v->type = Double;
		}

		v->value = a->value * b->value;
		break;
	case DIV:
		v->type = Double;
		if (b->value == 0) {
			printf("Division with 0 is not possible\n");
			exit(0);
		}
		v->value = a->value / b->value;
		break;
	case LS:
		v->type = Integer;
		v->value = a->value < b->value;
		break;
	case LEQ:
		v->type = Integer;
		v->value = a->value <= b->value;
		break;
	case GE:
		v->type = Integer;
		v->value = a->value > b->value;
		break;
	case GEQ:
		v->type = Integer;
		v->value = a->value >= b->value;
		break;
	case EQEQ:
		v->type = Integer;
		v->value = a->value == b->value;
		break;
	}

	freeVar(a);
	freeVar(b);
	return v;
}


void printValue(struct var* node) {
	int type = node->type;
	double value = node->value;
	char* valueStr = node->valueStr;

	switch (type) {
	case Integer:
		printf("%d\n", (int)node->value);
		break;
	case Character:
		printf("%c\n", (char)node->value);
		break;
	case Float:
		printf("%f\n", (float)node->value);
		break;
	case Double:
		printf("%f\n", (double)node->value);
		break;
	case String:
		printf("%s\n", (char*)node->valueStr);
		break;
	default:
		break;
	}

}

struct var* initializeVar() {
	struct var* v = (struct var*)malloc(sizeof(struct var));

	sprintf(v->id, "%s", "");
	sprintf(v->valueStr, "%s", "");

	v->value = 0;

	return v;
}

int main (void) {
    yyin = fopen("input", "r");
	return yyparse();
}

void yyerror (char *s) {fprintf (stderr, "%s\n", s);} // TODO