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

	char* identifier;
	double value;
	char* valueStr;

	int cnst;
};

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
void pushEmptyVariable(char*, int)
struct var* comp(struct var*, struct var*, char*);
void printValue(struct var*);

%}

%union {
	double num; 
	char id[20]; 
	int type_id; 
	char string[1000]; 
	struct var* strct;
}         /* Yacc definitions */
%start lines
%token print

%type <type_id> DATA_TYPE
%token <type_id> Integer Float Double Character Bool String
%token Const

%token GEQ
%token LEQ
%token AND
%token OR
%token EQ

%type<num> stat
%token IF
%type<num> smtm smtm_type smtm_types smtm_fun ELSE_ ELIF_ ELIF_S
%token ELSE
%token ELIF

%token FUN RETURN
%type<num> FUNCTION

%token <string> String_Value Character_Value

%token exit_command
%token <num> number
%token <id> identifier
%type <num> line lines

%type <strct> exp term 

%type <num> assignment

%nonassoc IF ELSE ELIF

%right '='

%left '-' '+'
%left '/' '*'

%left EQ
%left GEQ LEQ '<' '>'

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

assignment  : DATA_TYPE identifier	 					{pushEmptyVariable($2, $1);}
			| DATA_TYPE identifier '=' exp  			{pushVariable($2, $4, $1);}
		
			| Const DATA_TYPE identifier '=' exp  		{pushVariable($3, $5, $2);} // TODO const

			| DATA_TYPE identifier '[' exp ']'			{pushEmptyVariable($2, $1);} // TODO exp

			| identifier '=' exp   						{updateValue($1, $3);}
			;


exp    	: term                     	{$$ = $1;}
     	| '(' exp ')'			   	{$$ = $2;}
       	| exp '+' exp              	{$$ = comp($1, $3, "+");} // todo if it's identifier
       	| exp '-' exp              	{$$ = comp($1, $3, "-");}
       	| exp '*' exp              	{$$ = comp($1, $3, "*");}
        | exp '/' exp          	   	{$$ = comp($1, $3, "/");} // TODO 0 division

		| exp AND exp              	{$$ = comp($1, $3, "&&");}
		| exp OR exp               	{$$ = comp($1, $3, "||");}
		| exp '<' exp 				{$$ = comp($1, $3, "<") ;}
		| exp '>' exp 				{$$ = comp($1, $3, ">");}
		| exp LEQ exp 				{$$ = comp($1, $3, "<=");}
		| exp GEQ exp 				{$$ = comp($1, $3, ">=");}
		| exp EQ exp 				{$$ = comp($1, $3, "==");}
		;


term	: identifier			{$$ = temporaryPointVar($1);} 
   		| number                {$$ = temporaryPointNum($1, Double);}
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
	struct var *v = (struct var*) malloc(sizeof(var));

	v->value = val;
	v->type = type;

	return v;
}

struct var* temporaryPointStr(void* val, int type) {
	struct var *v = (struct var*) malloc(sizeof(var));

	v->type = type;

	if (type == String) {
		v->valueStr = (char*)val;
	} else {
		v->value = ((char*)val)[0];
	}

	return v;
}

struct var* temporaryPointVar(char* identifier) {
	int i = getVariableIndex(identifier);

	if (i == -1) {
		printf("Variable %s was not declared in this scope\n", identifier);
		exit(0);
	}

	struct var *v = &variables[i];
	struct var *exp = (struct var*)malloc(sizeof(struct var));

	exp->type = v->type;

	if (v->type == String) {
		exp->valueStr = v->valueStr;
	} else {
		exp->value = v->value;
	}

	return exp;
}

void freeVar(struct var* v) {
	free(v);
}

int getVariableIndex(char* varName) {
	for (int i = 0; i < totalVar; i++) {
		if (strcmp(varName, variables[i].identifier) == 0) {
			return i;
		}
	}

	return -1;
} 

void updateValue(char* identifier, struct var* exp) {
	int i = getVariableIndex(identifier);

	if (i == -1) {
		printf("Variable %s was not declared in this scope\n", symbol);
		exit(0);
	} 

	struct var *vr = variables + i;

	if (vr->type == String) {
		vr->valueStr = v->valueStr;
	} else {
		vr->value = v->value;
	}

}

void FloatingPointException(int val) {
	if(!val) {
		printf("Nu se poate imparti la 0\n");
		exit(0);
	}
}

void pushEmptyVariable(char* identifier, int type) {
	int i = getVariableIndex(identifier);

	if (i != -1) {
		printf("The variable %s was already declared here\n", symbol);
		exit(0);
	}

	struct var *v = variables[totalVar];
	
	v->identifier = identifier;
	v->type = type;

	if (type == String) {
		v->value = 0;
	} else {
		char *str = malloc(1);
		sprintf(str, "%s", "");
		v->valueStr = exp->str;
	}

	totalVar++;
}

void pushVariable(char* identifier, int type, struct var* exp) {
	int i = getVariableIndex(identifier);

	if (i != -1) {
		printf("The variable %s was already declared here\n", symbol);
		exit(0);
	}

	var *v = variables[totalVar];
	
	v->identifier = identifier;
	v->type = type;

	if (type == String) {
		v->value = exp->value;
	} else {
		v->valueStr = exp->valueStr;
	}

	freeVar(exp);
	totalVar++;
}

struct var* comp(struct var* a, struct var* b, char* type) {
	// TODO
}


void printValue(struct var* exp) {



	// 
}

int main (void) {
    yyin = fopen("input", "r");
	return yyparse();
}

void yyerror (char *s) {fprintf (stderr, "%s\n", s);} // TODO