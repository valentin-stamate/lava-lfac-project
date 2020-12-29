%{
#include <stdio.h>
#include <string.h>

#include "y.tab.h"

void yyerror (char *s);
int yylex();
%}
%%
"print"				   {return print;}
"exit"				   {return exit_command;}
"Integer"              {yylval.type_id = Integer; return Integer;}
"Float"                {yylval.type_id = Float; return Float;}
"Double"               {yylval.type_id = Double; return Double;}
"Character"            {yylval.type_id = Character; return Character;}
[a-zA-Z]+			   {strcpy(yylval.id, yytext); return identifier;}
[0-9]+                 {yylval.num = atoi(yytext); return number;}
[ \t\n]                ;
[-+=*/;]           	   {return yytext[0];}
[()]                   ;
.                      {ECHO; yyerror ("unexpected character");}

%%
int yywrap (void) {return 1;}