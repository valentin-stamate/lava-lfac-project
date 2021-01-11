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

#define TYPE_NORMAL 1
#define TYPE_ARRAY 2
#define TYPE_FUNCTION 3

struct var {
	char id[100];
	int var_type;

	// normal type
	int type;
	int cnst;

	// array
	int arraySize;
	double array[100];
	char arrayStr[100][1000];
	
	// function
	int parameterNum;
	int parameterTypes[10];
};

struct parameter {
	int paramNum;
	int parameterTypes[10];
};

struct var* initializeVar();

#define BBLK "\e[1;30m"
#define BRED "\e[1;31m"
#define BGRN "\e[1;32m"
#define BYEL "\e[1;33m"
#define BBLU "\e[1;34m"
#define BMAG "\e[1;35m"
#define BCYN "\e[1;36m"
#define BWHT "\e[1;37m"
#define RESET "\e[0m"

int totalVar = 0;
struct var variables[100];

struct var* temporaryPointNum(double, int);
struct var* temporaryPointStr(void*, int);
struct var* temporaryPointVar(char*);
struct var* temporaryPointArr(char*, struct var*);
struct var* temporaryPointFun(char*, struct parameter*);

void freeVar(struct var* v);
int getVariableIndex(char*);
void updateValue(char*, struct var*);
void FloatingPointException(int);
void pushVariable(char*, int, struct var*);
void pushVariableConst(char*, int, struct var*);
void pushEmptyVariable(char*, int);
void pushStructVariable(char*);
struct var* comp(struct var*, struct var*, int);
void printValue(struct var*);
void print_simbol_table(struct var*,int);

void pushArray(char*, int, struct var*);
void updateArrValue(char*, struct var*, struct var*);
void Eval_function(struct var*);

void pushParam(struct parameter*, int);
struct parameter* initializeParam(int);
void pushFunction(char*, int, struct parameter*);
char* defToDataType(int);
%}

%union {
	double num; 
	char string[1000]; 
	int type_id; 
	struct var* strct;
	struct parameter* funParam;
}     

%start program

%token print

%type <type_id> DATA_TYPE
%token <type_id> Integer Float Character Bool String
%token Const

%token GEQ LEQ AND OR EQEQ LS GE
%token PLUS MINUS PROD DIV EQUAL

%type<num> stat
%token IF WHILE FOR
%type<num> smtm smtm_type smtm_types smtm_fun ELSE_ ELIF_ ELIF_S 
%token ELSE
%token ELIF

%token FUN RETURN DEFINE_TYPE
%type<num> FUNCTION

%token <string> String_Value Character_Value

%type <type_id> paramentru
%type<funParam> lista_param more_params fun_call_list more_call_param

%token EVAL

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


program : lines { print_simbol_table(variables,totalVar);printf(BGRN "Program corect sintactic\n\n" RESET); }
		;

lines   : line			 			{;}
		| lines line				{;}
		;

line 	: assignment ';'								{;}
		| exit_command ';'								{exit(EXIT_SUCCESS);}
		| print exp ';'									{printValue($2);}
		| stat 											{;}
		| FUNCTION 				   						{;}
		| EVAL '(' exp ')' ';'          				{Eval_function($3);}
		| DEFINE_TYPE '{' ELEMENTS '}' IDENTIFIER ';'   {pushStructVariable($5);}
		;

ELEMENTS : DATA_TYPE IDENTIFIER ';' ELEMENTS	{;}
		 |										{;}
		 ;

DATA_TYPE   : Integer   	 {$$ = $1;}
			| Float			 {$$ = $1;}
			| Character 	 {$$ = $1;}
			| Bool 	 		 {$$ = $1;}
			| String		 {$$ = $1;}
			;

assignment  : DATA_TYPE IDENTIFIER	 					{pushEmptyVariable($2, $1);}
			| DATA_TYPE IDENTIFIER EQUAL exp  			{pushVariable($2, $1, $4);}
		
			| Const DATA_TYPE IDENTIFIER EQUAL exp  	{pushVariableConst($3, $2, $5);}

			| DATA_TYPE IDENTIFIER '[' exp ']'			{pushArray($2, $1, $4);}

			| IDENTIFIER EQUAL exp   					{updateValue($1, $3);}
			| IDENTIFIER '[' exp ']' EQUAL exp			{updateArrValue($1, $3, $6);}
			;


exp    	: term                     	{$$ = $1;}
     	| '(' exp ')'			   	{$$ = $2;}
       	| exp PLUS exp              {$$ = comp($1, $3, PLUS);}
       	| exp MINUS exp             {$$ = comp($1, $3, MINUS);}
       	| exp PROD exp              {$$ = comp($1, $3, PROD);}
        | exp DIV exp          	   	{$$ = comp($1, $3, DIV);}

		| exp AND exp              	{$$ = comp($1, $3, AND);}
		| exp OR exp               	{$$ = comp($1, $3, OR);}
		| exp LS exp 				{$$ = comp($1, $3, LS) ;}
		| exp GE exp 				{$$ = comp($1, $3, GE);}
		| exp LEQ exp 				{$$ = comp($1, $3, LEQ);}
		| exp GEQ exp 				{$$ = comp($1, $3, GEQ);}
		| exp EQEQ exp 				{$$ = comp($1, $3, EQEQ);}
		;


term	: IDENTIFIER						{$$ = temporaryPointVar($1);} 
		| IDENTIFIER '[' exp ']'			{$$ = temporaryPointArr($1, $3);}
		| IDENTIFIER '(' fun_call_list ')' 	{$$ = temporaryPointFun($1, $3);}
   		| number                			{$$ = temporaryPointNum($1, Integer);}
		| number_r							{$$ = temporaryPointNum($1, Float);}
		| Character_Value					{$$ = temporaryPointStr($1, Character);}
		| String_Value						{$$ = temporaryPointStr($1, String);}
        ;

fun_call_list	: more_call_param			{$$ = $1;}
				|							{$$ = initializeParam(0);}
				;

more_call_param : exp						{$$ = initializeParam($1->type);}
				| more_call_param ',' exp	{pushParam($$, $3->type);}
				;

stat	: IF '(' exp ')' smtm				{;}
		| IF '(' exp ')' smtm ELIF_S ELSE_ 	{;}
		| IF '(' exp ')' smtm ELSE_ 		{;}
		| IF '(' exp ')' smtm ELIF_S	 	{;}
		| WHILE '(' exp ')' smtm			{;}
		| FOR '(' IDENTIFIER EQUAL exp ';' exp ';' IDENTIFIER EQUAL exp ')' smtm					{;}
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



FUNCTION 	: DATA_TYPE FUN IDENTIFIER '(' lista_param ')' smtm_fun 		{pushFunction($3, $1, $5);}
			;

lista_param : more_params													{$$ = $1;}
			| 																{$$ = initializeParam(0);}
			;

more_params : paramentru													{$$ = initializeParam($1);}
			| more_params ',' paramentru									{pushParam($$, $3);}
			;

paramentru  : DATA_TYPE IDENTIFIER											{$$ = $1;}
			;


smtm_fun	: '{' smtm_types RETURN exp ';' '}' 		{;}
			| '{' RETURN exp ';' '}'				{;}
			;

%%
void pushStructVariable(char*id)
{
	int i = getVariableIndex(id);

	if (i != -1) {
		printf(BRED "The variable " BBLU "%s" BRED " was already declared here\n", id);
		exit(0);
	}

	struct var *v = variables + totalVar;
	
	sprintf(v->id, "%s", id);
	v->type = 0;

	totalVar++;
}

void pushFunction(char* id, int retType, struct parameter* p) {
	int i = getVariableIndex(id);

	if (i != -1) {
		printf(BRED "Name for function " BBLK "%s" BRED " was already taken.\n" RESET, id);
		exit(0);
	}

	struct var *v = variables + totalVar;
	
	sprintf(v->id, "%s", id);
	v->var_type = TYPE_FUNCTION;
	v->type = retType;
	v->parameterNum = p->paramNum;
	for (int i = 0; i < p->paramNum; i++) {
		v->parameterTypes[i] = p->parameterTypes[i];
	}

	free(p);
	totalVar++;
}

void pushParam(struct parameter* p, int type) {
	p->parameterTypes[p->paramNum++] = type;	
}

struct parameter* initializeParam(int type) {
	struct parameter *p = (struct parameter*)malloc(sizeof(struct parameter));
	if (type == 0) {
		p->paramNum = 0;
		return p;
	}

	p->paramNum = 1;
	p->parameterTypes[0] = type;

	return p;
}

void Eval_function(struct var* x)
{
  if(x->type == Integer)
  		printf(BGRN "%d\n" RESET,(int)x->array[0]);
  else
  {
	  	printf(BYEL "Eval function must have an integer type parameter.\n" RESET);
		exit(0);
  }
   
}

void print_simbol_table(struct var* v,int n)
{
	FILE *fd;
	fd = fopen("symbol_table.txt", "w");
	if(fd == NULL)
	{
		char buffer[100];
		sprintf(buffer, "Nu pot deschide fisierul symbol_table.txt.");
		yyerror(buffer);
		exit(0);
	}
 	
 	for(int i=0;i<n;i++)
	{
	    if(v[i].var_type != TYPE_ARRAY && v[i].var_type != TYPE_FUNCTION)
		{
			fprintf(fd,"nume : %s  ",v[i].id);
			switch (v[i].type) {
			case Integer:
				fprintf(fd, "tip = Integer valoare = %d  ", (int)v[i].array[0]);
				break;
			case Character:
				fprintf(fd, "tip = Character valoare = '%c' ", (char)v[i].array[0]);
				break;
			case Float:
				fprintf(fd, "tip = Float valoare = %f ", (float)v[i].array[0]);
				break;
			case String:
				fprintf(fd, "tip = String valoare = \"%s\" ", (char*)v[i].arrayStr[0]);
				break;
			case Bool:
				fprintf(fd, "tip = Bool valoare = %d ", (int)v[i].array[0]);
				break;
			case 0:
				fprintf(fd, "tip = User Defined Type\n");
				break;
			default:
				break;
			}
			if(v[i].type!=0)
				if(v[i].cnst)
					fprintf(fd, "constant \n");
				else
					fprintf(fd, "not constant \n");
		}
		else if(v[i].var_type != TYPE_FUNCTION)
		{
			fprintf(fd,"nume : %s  ",v[i].id);
			switch (v[i].type) {
			case Integer:
				fprintf(fd, "tip = Integer Array ");
				for(int j=0;j<v[i].arraySize;j++)
				{
				 	fprintf(fd,"%s[%d] = %d  ", v[i].id, j, (int)v[i].array[j]);
				}
				break;
			case Character:
				fprintf(fd, "tip = Chraracter Array ");
				for(int j=0;j<v[i].arraySize;j++)
				{
				 	fprintf(fd,"%s[%d] = %c  ", v[i].id, j, (char)v[i].array[j]);
				}
				break;
			case Float:
				fprintf(fd, "tip = Float Array ");
				for(int j=0;j<v[i].arraySize;j++)
				{
				 	fprintf(fd,"%s[%d] = %f  ", v[i].id, j, (float)v[i].array[j]);
				}
				break;
			case String:
				fprintf(fd, "tip = String Array");
				for(int j=0;j<v[i].arraySize;j++)
				{
					fprintf(fd," %s[%d] = \"%s\" ", v[i].id, j, (char*)v[i].arrayStr[j]);
				}
				break;
			case Bool:
				fprintf(fd, "tip = Bool Array ");
				for(int j=0;j<v[i].arraySize;j++)
				{
				 	fprintf(fd,"%s[%d] = %d  ", v[i].id, j, (int)v[i].array[j]);
				}
				break;
			default:
				break;
			}
			fprintf(fd,"\n");
		}

	}
	fprintf(fd,"\n");
    for(int i=0;i<n;i++)
	{
		if(v[i].var_type == TYPE_FUNCTION)
		{
            fprintf(fd, "Function : ");
			fprintf(fd, "%s %s", defToDataType(v[i].type), v[i].id);
			int nr = v[i].parameterNum;
			if (nr != 0) 
			{   
				fprintf(fd,  " (");
				for (int j = 0; j < nr-1 ; j++)
				{
					fprintf(fd, "%s, ", defToDataType(v[i].parameterTypes[j]));
				}
				fprintf(fd, "%s", defToDataType(v[i].parameterTypes[nr-1]));
				fprintf(fd,  ") ");
			}
			else
			{
			 fprintf(fd, "();");
			}

			fprintf(fd, "\n");
			
		}
	}
}

struct var* temporaryPointNum(double val, int type) {
	struct var *v = initializeVar();

	v->array[0] = val;
	v->type = type;

	return v;
}

struct var* temporaryPointStr(void* val, int type) {
	struct var *v = initializeVar();

	v->type = type;

	if (type == String) {
		sprintf(v->arrayStr[0], "%s", (char*)val);
	} else {
		v->array[0] = ((char*)val)[0];
	}

	return v;
}

struct var* temporaryPointFun(char* id, struct parameter* pr) {
	int i = getVariableIndex(id);

	if (i == -1) {
		printf(BRED "Function " BYEL "%s" BRED " was not declared in this scope.\n", id);
		exit(0);
	}

	struct var* v = variables + i;

	if (v->var_type != TYPE_FUNCTION) {
		printf(BBLU "%s" BRED " should be a function, not a variable.\n" RESET, v->id);
		exit(0);
	}

	if (v->parameterNum != pr->paramNum) {
		printf(BRED "The number of variables should match for function " BBLU "%s" BRED ".\n" RESET, v->id);
		exit(0);
	}

	int n = v->parameterNum;

	int *funParams = v->parameterTypes;
	int *callParams = pr->parameterTypes;

	for (int i = 0; i < n; i++) {
		if (funParams[i] != callParams[i]) {
			printf(BBLU "%s" BRED " parameters should match function definition.\n" RESET, v->id);
			printf(BRED "Parameter " BMAG "%d" BRED " is " BBLU "%s" BBLU " but in definition is " BBLU "%s" BRED ".\n" RESET, i + 1, defToDataType(callParams[i]), defToDataType(funParams[i]));
			exit(0);
		}
	}

	v = initializeVar();
	v->type = Integer;
	v->array[0] = 0;

	return v;
}

struct var* temporaryPointVar(char* id) {
	int i = getVariableIndex(id);

	if (i == -1) {
		printf(BYEL "%s" BRED " was not declared in this scope.\n", id);
		exit(0);
	}
	struct var* v = variables + i;
	
	if (v->var_type == TYPE_FUNCTION) {
		v->array[0] = 0;
	}

	return v;
}

struct var* temporaryPointArr(char* id, struct var* node) {
	int i = getVariableIndex(id);

	if (i == -1) {
		printf(BYEL "%s" BRED " was not declared in this scope.\n", id);
		exit(0);
	}

	struct var *v = variables + i;

	if (v->var_type != TYPE_ARRAY) {
		printf(BRED "Varialbe " BBLU "%s" BRED " is not an array type.\n" RESET, v->id);
		exit(0);
	}

	if (node->type == String) {
		printf(BRED"This array type cannot be accessed with a string expression.\n" RESET);
		exit(0);
	}

	int n = (int)node->array[0];

	if (n < 0) {
		printf(BRED "Array index should be more than 0 but it's " BMAG "%d" BRED ".\n" RESET, n);
		exit(0);
	}

	if (n >= v->arraySize) {
		printf(BRED "Array size exceded for variable " BBLU "%s" BRED " from expression: " BMAG "%d" BRED ", where maximum index is " BMAG "%d" BRED ".\n" RESET, id, n, v->arraySize - 1);
		exit(0);
	}

	struct var *exp = initializeVar();

	exp->type = v->type;

	if (v->type == String) {
		sprintf(exp->arrayStr[0], "%s", v->arrayStr[n]);
	} else if (v->type == Bool) {
		exp->array[0] = v->array[n] != 0;
	} else {
		exp->array[0] = v->array[n];
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
		printf(BYEL "%s" BRED " was not declared in this scope.\n", id);
		exit(0);
	} 

	struct var *vr = variables + i;
	
	if (vr->var_type == TYPE_FUNCTION) {
		printf(BRED"Function " BBLU "%s" BRED " cannot be changed.\n" RESET, vr->id);
		exit(0);
	}

	if (vr->var_type == TYPE_ARRAY && exp->var_type != TYPE_ARRAY) {
		printf(BRED "Variable " BBLU "%s" BRED " is an array type but the expression is not.\n" RESET, vr->id);
		exit(0);
	}

	if (vr->var_type != TYPE_ARRAY && exp->var_type == TYPE_ARRAY) {
		printf(BRED "Variable " BBLU "%s" BRED " is a normal type but expression is an array.\n" RESET, vr->id);
		exit(0);
	}
	
	if (vr->type == String && exp->type != String || vr->type != String && exp->type == String) {
		printf(BRED "Data types should match.\n" RESET);
		exit(0);
	}
	
	if(vr->cnst)
	{
		printf(BRED "Constat variable " BBLU "%s" BRED " cannot be modified.\n", id);
		exit(0);
	} 

	if (vr->var_type == TYPE_ARRAY && exp->var_type == TYPE_ARRAY) {

		int n = vr->arraySize;
		int m = exp->arraySize;

		for (int i = 0; i < n && i < m; i++) {
			if (vr->type == String) {
				sprintf(vr->arrayStr[i], "%s", exp->arrayStr[i]);
			} else if (vr->type == Bool) {
				vr->array[i] = exp->array[i] != 0;
			} else {
				vr->array[i] = exp->array[i];
			}
		}

		return;
	}

	if (vr->type == String) {
		sprintf(vr->arrayStr[0], "%s", exp->arrayStr[0]);
	} else if (vr->type == Bool) {
		vr->array[0] = exp->array[0] != 0;
	} else {
		vr->array[0] = exp->array[0];
	}

}

void updateArrValue(char* id, struct var* exp_1, struct var* exp_2) {
	int i = getVariableIndex(id);

	if (i == -1) {
		printf(BYEL "%s" BRED " was not declared in this scope.\n" RESET , id);
		exit(0);
	}

	struct var *v = variables + i;

	if (v->var_type == TYPE_FUNCTION) {
		printf(BRED "Invalid expression for function " BBLU "%s" BRED ".\n" RESET, v->id);
		exit(0);
	}

	if (exp_1->type == String) {
		printf(BRED "This array type cannot be accessed with a string expression.\n" RESET);
		exit(0);
	}

	int n = (int)exp_1->array[0];

	if (n < 0) {
		printf(BRED "Array index should be more than " BMAG "0" BRED " but it's " BMAG "%d" BRED ".\n" RESET, n);
		exit(0);
	}

	if (n >= v->arraySize) {
		printf(BRED"Array size exceded for %s: %d, where maximum index is %d.\n" RESET, id, n, v->arraySize - 1);
		exit(0);
	}

	if (v->type == String && exp_2->type != String || v->type != String && exp_2->type == String) {
		printf(BRED "Data type should match for variable " BBLU "%s" BWHT "[" BMAG "%d" BWHT "]" BRED ".\n" RESET, id, n);
		exit(0);
	}

	if (v->type == String) {
		sprintf(v->arrayStr[n], "%s", exp_2->arrayStr[0]);
	} else if (v->type == Bool) {
		v->array[n] = exp_2->array[0] != 0;
	} else {
		v->array[n] = exp_2->array[0];
	}
}

void pushEmptyVariable(char* id, int type) {
	int i = getVariableIndex(id);

	if (i != -1) {
		printf(BRED "The variable " BBLU "%s" BRED " was already declared here\n", id);
		exit(0);
	}

	struct var *v = variables + totalVar;
	
	sprintf(v->id, "%s", id);
	v->type = type;

	if (type == String) {
		sprintf(v->arrayStr[0], "%s", "");
	} else {
		v->array[0] = 0;
	}

	totalVar++;
}

void pushVariable(char* id, int type, struct var* exp) {
	int i = getVariableIndex(id);

	if (i != -1) {
		printf(BRED "The variable " BBLU "%s" BRED " was already declared here\n", id);
		exit(0);
	}

	struct var *v = variables + totalVar;
	
	sprintf(v->id, "%s", id);
	v->type = type;

	if (type == String) {
		sprintf(v->arrayStr[0], "%s", exp->arrayStr[0]);
	} else if (type == Bool) {
		v->array[0] = exp->array[0] != 0;
	} else {
		v->array[0] = exp->array[0];
	}

	freeVar(exp);
	totalVar++;
}

void pushArray(char* id, int type, struct var* exp) {
	int i = getVariableIndex(id);

	if (i != -1) {
		printf(BRED "The variable " BBLU "%s" BRED " was already declared here\n", id);
		exit(0);
	}

	if (exp->type == String) {
		printf(BYEL "Array types cannot be declared with string expressions.\n" RESET);
		exit(0);
	}

	int n = (int)exp->array[0];

	if (n <= 0) {
		printf(BYEL "The array size should be at least 1.\n" RESET);
		exit(0);
	}

	struct var *v = variables + totalVar;

	if (v->type == String && exp->type != String || v->type != String && exp->type == String) {
		printf(BYEL "Data types should match.\n" RESET);
		exit(0);
	}

	sprintf(v->id, "%s", id);
	v->type = type;
	v->var_type = TYPE_ARRAY;
	v->arraySize = n;

	totalVar++;
}

void pushVariableConst(char* id, int type, struct var* exp) {
	int i = getVariableIndex(id);

	if (i != -1) {
		printf("The variable %s was already declared here\n", id);
		exit(0);
	}

	struct var *v = variables + totalVar;
	
	sprintf(v->id, "%s", id);
	v->type = type;

	if (type == String) {
		sprintf(v->arrayStr[0], "%s", exp->arrayStr[0]);
	} else if (type == Bool) {
		v->array[0] = exp->array[0] != 0;
	} else {
		v->array[0] = exp->array[0];
	}
    v->cnst=1;
	freeVar(exp);
	totalVar++;
}

struct var* comp(struct var* a, struct var* b, int op_type) {
	
	struct var* v = initializeVar();
	double c;

	int n;

	switch (op_type) {
	case PLUS:;
		if (a->type == String && b->type == String) {
			v->type = String;
			strcpy(v->arrayStr[0], "");
			strcat(v->arrayStr[0], a->arrayStr[0]);
			strcat(v->arrayStr[0], b->arrayStr[0]);
			break;
		}

		if (a->type != String && b->type == String) {
			v->type = String;
			strcpy(v->arrayStr[0], "");
			sprintf(v->arrayStr[0], "%f", (float)a->array[0]);
			strcat(v->arrayStr[0], b->arrayStr[0]);
			break;
		}

		if (a->type == String && b->type != String) {
			v->type = String;
			strcpy(v->arrayStr[0], "");

			strcat(v->arrayStr[0], a->arrayStr[0]);
			char bfr[10];
			sprintf(bfr, "%f", (float)b->array[0]);
			strcat(v->arrayStr[0], bfr);
			break;
		}

		if (a->type == Character) {
			v->type = Character;
			v->array[0] = (int)(a->array[0] + b->array[0]);
			break;
		}

		c = a->array[0] + b->array[0];

		if (c == (int)c) {
			v->type = Integer;
			v->array[0] = (int)c;
		} else {
			v->type = Float;
			v->array[0] = c;
		}
		break;
	case MINUS:;

		if (a->type == Character) {
			v->type = Character;
			v->array[0] = (int)(a->array[0] - b->array[0]);
			break;
		}

		c = a->array[0] - b->array[0];

		if (c == (int)c) {
			v->type = Integer;
			v->array[0] = (int)c;
		} else {
			v->type = Float;
			v->array[0] = c;
		}
		break;
	case PROD:;
		c = a->array[0] * b->array[0];

		if (c == (int)c) {
			v->type = Integer;
			v->array[0] = (int)c;
		} else {
			v->type = Float;
			v->array[0] = c;
		}
		break;
	case DIV:;
	    double c = a->array[0] / b->array[0];
		if (c == (int)c) 
			v->type = Integer;
		else
			v->type = Float;
		if (b->array[0] == 0) {
			printf(BRED "Division with 0 is not possible.\n" RESET);
			exit(0);
		}

	    c = a->array[0] / b->array[0];

		if (c == (int)c) { 
			v->type = Integer;
			v->array[0] = (int)c;
		} else {
			v->type = Float;
			v->array[0] = c;
		}
		break;
	case LS:;
		if (a->type == String && b->type == String) {
			n = strcmp(a->arrayStr[0], b->arrayStr[0]);
			v->array[0] = n == -1;
		} else {
			v->array[0] = (int)(a->array[0] < b->array[0]);
		}
		v->type = Integer;
		break;
	case LEQ:;
		if (a->type == String && b->type == String) {
			n = strcmp(a->arrayStr[0], b->arrayStr[0]);
			v->array[0] = n == -1 || n == 0;
		} else {
			v->array[0] = (int)(a->array[0] <= b->array[0]);
		}
		v->type = Integer;
		break;
	case GE:;
		if (a->type == String && b->type == String) {
			n = strcmp(a->arrayStr[0], b->arrayStr[0]);
			v->array[0] = n == 1;
		} else {
			v->array[0] = (int)(a->array[0] > b->array[0]);
		}
		v->type = Integer;
		break;
	case GEQ:;
		if (a->type == String && b->type == String) {
			n = strcmp(a->arrayStr[0], b->arrayStr[0]);
			v->array[0] = n == 1 || n == 0;
		} else {
			v->array[0] = (int)(a->array[0] >= b->array[0]);
		}
		v->type = Integer;
		break;
	case EQEQ:;
		if (a->type == String && b->type == String) {
			n = strcmp(a->arrayStr[0], b->arrayStr[0]);
			v->array[0] = n == 0;
		} else {
			v->array[0] = (int)(a->array[0] == b->array[0]);
		}
		v->type = Integer;
		break;
	}

	freeVar(a);
	freeVar(b);
	return v;
}


void printValue(struct var* node) {
	int type = node->type;
	int n;

	switch (type) {
	case Integer:
		if (node->var_type == TYPE_ARRAY) {
			n = node->arraySize;
			printf("{");
			for (int i = 0; i < n - 1; i++) {
				printf("%d, ", (int)node->array[i]);
			}
			printf("%d", (int)node->array[n - 1]);
			printf("}\n");
			break;
		}
		if (node->var_type == TYPE_NORMAL)
			printf("%d\n", (int)node->array[0]);
		break;
	case Character:
		if (node->var_type == TYPE_ARRAY) {
			n = node->arraySize;
			printf("{");
			for (int i = 0; i < n - 1; i++) {
				printf("'%c', ", (char)node->array[i]);
			}
			printf("'%c'", (char)node->array[n - 1]);
			printf("}\n");
			break;
		}
		if (node->var_type == TYPE_NORMAL)
			printf("'%c'\n", (char)node->array[0]);
		break;
	case Float:
		if (node->var_type == TYPE_ARRAY) {
			n = node->arraySize;
			printf("{");
			for (int i = 0; i < n - 1; i++) {
				printf("%f, ", (float)node->array[i]);
			}
			printf("%f", (float)node->array[n - 1]);
			printf("}\n");
			break;
		}
		if (node->var_type == TYPE_NORMAL)
			printf("%f\n", (float)node->array[0]);
		break;
	case String:
		if (node->var_type == TYPE_ARRAY) {
			n = node->arraySize;
			printf("{");
			for (int i = 0; i < n - 1; i++) {
				printf("\"%s\", ", node->arrayStr[i]);
			}
			printf("\"%s\"", node->arrayStr[n - 1]);
			printf("}\n");
			break;
		}
		if (node->var_type == TYPE_NORMAL)
			printf("\"%s\"\n", node->arrayStr[0]);
		break;
	case Bool:
		if (node->var_type == TYPE_ARRAY) {
			n = node->arraySize;
			printf("{");
			for (int i = 0; i < n - 1; i++) {
				printf("\"%d\", ", (int)node->array[i]);
			}
			printf("\"%d\"", (int)node->array[n - 1]);
			printf("}\n");
			break;
		}
		if (node->var_type == TYPE_NORMAL)
			printf("%d\n", (int)node->array[0]);
		break;
	default:
		break;
	}

	if (node->var_type == TYPE_FUNCTION) {
		printf("%s %s", defToDataType(node->type), node->id);
		n = node->parameterNum;
		if (n != 0) {
			printf(" -> {");
			for (int i = 0; i < n - 1; i++) {
				printf("%s, ", defToDataType(node->parameterTypes[i]));
			}
			printf("%s}", defToDataType(node->parameterTypes[n - 1]));
		}

		printf("\n");
	}

}

struct var* initializeVar() {
	struct var* v = (struct var*)malloc(sizeof(struct var));

	sprintf(v->id, "%s", "");
	sprintf(v->arrayStr[0], "%s", "");

	v->array[0] = 0;
	v->var_type = TYPE_NORMAL;
	v->type = 0;

	return v;
}

char* defToDataType(int n) {
	switch (n) {
	case Bool:
		return "Bool";
		break;
	case Character:
		return "Char";
		break;
	case Integer:
		return "Integer";
		break;
	case Float:
		return "Float";
		break;
	case String:
		return "String";
		break;
	}
	return "";
}

int main (void) {

	for (int i = 0; i < 100; i++) {
		variables[i].var_type = TYPE_NORMAL;
	}

    yyin = fopen("input", "r");
	return yyparse();
}

void yyerror (char *s) 
{
	printf (BRED "Error: %s line %d\n" RESET, s, yylineno);
}