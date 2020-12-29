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

#define MAX_VAR 100

char symbols[MAX_VAR][20];
double symbols_values[MAX_VAR];

int totalVar = 0;

int symbolVal(char*);
void updateSymbolVal(char*, double);
void FloatingPointException(int);
void push(char*, double);
%}

%union {double num; char id[20];}         /* Yacc definitions */
%start line
%token print
%token integer
%token exit_command
%token <num> number
%token <id> identifier
%type <num> line exp term 
%type <id> assignment

%%

/* descriptions of expected inputs     corresponding actions (in C) */

line    : assignment ';'		{;}
		| exit_command ';'		{exit(EXIT_SUCCESS);}
		| print exp ';'			{printf("Printing %f %d\n", $2, totalVar);}
		| line assignment ';'	{;}
		| line print exp ';'	{printf("Printing %f %d\n", $3, totalVar);}
		| line exit_command ';'	{exit(EXIT_SUCCESS);}
        ;

assignment  : identifier '=' exp  { updateSymbolVal($1,$3); }
			| integer identifier '=' exp { push($2, $4); }
			;
exp    	: term                  {$$ = $1;}
       	| exp '+' term          {$$ = $1 + $3;}
       	| exp '-' term          {$$ = $1 - $3;}
     	| '(' exp '+' term ')'         {$$ = $2 + $4;}
       	| '(' exp '-' term ')'         {$$ = $2 - $4;}
       	| exp '*' term          {$$ = $1 * $3;}
     	| '(' exp '*' term ')'         {$$ = $2 * $4;}
        | exp '/' term          {FloatingPointException($3);$$ = $1 / $3;}
     	| '(' exp '/' term ')'         {FloatingPointException($4);$$ = $2 / $4;}
       	;
term   	: number                {$$ = $1;}
		| identifier			{$$ = symbolVal($1);} 
        ;
%%

int computeSymbolIndex(char* varName) {
	for (int i = 0; i < totalVar; i++) {
		if (strcmp(varName, symbols[i]) == 0) {
			return i;
		}
	}
	return -1;
} 



int symbolVal(char* symbol) {
	int i = computeSymbolIndex(symbol);
	return symbols_values[i];
}

void updateSymbolVal(char* symbol, double val) {
	int i = computeSymbolIndex(symbol);

	if (i == -1) {
		printf("Variable %s was not declared in this scope\n", symbol);
		exit(0);
	}

	symbols_values[i] = val;
}

void FloatingPointException(int val)
{
	if(!val)
    	{
			printf("Nu se poate imparti la 0\n");
		    exit(0);
		}
}

void push(char* symbol, double val) {
	int i = computeSymbolIndex(symbol);

	if (i != -1) {
		printf("The variable %s was already declared here\n", symbol);
		exit(0);
	}

	sprintf(symbols[totalVar], "%s", symbol);
	symbols_values[totalVar] = val;
	totalVar++;

}

int main (void) {

	for (int i = 0; i < MAX_VAR; i++) {
		strcpy(symbols[i], "");
		symbols_values[i] = 0;
	}

    yyin = fopen("input", "r");

	return yyparse();
}

void yyerror (char *s) {fprintf (stderr, "%s\n", s);} 