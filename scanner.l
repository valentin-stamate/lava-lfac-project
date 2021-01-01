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
"Char"                 {yylval.type_id = Character; return Character;}
"String"               {yylval.type_id = String; return String;}
"Bool"                 {yylval.type_id = Bool; return Bool;}
"<="                   {return leq;}
">="                   {return geq;}
"&&"                   {return AND;}
"||"                   {return OR;}
[()<>]                 {return yytext[0];}
[a-zA-Z]+			   {strcpy(yylval.id, yytext); return identifier;}
["].*["]               {strcpy(yylval.string, yytext + 1); yylval.string[strlen(yytext + 1) - 1] = '\0'; return String_Value;}
['].[']                {yylval.num = (int)yytext[1]; return Character_Value;}
[0-9]+                 {yylval.num = atoi(yytext); return number;}
[ \t\n]                ;
[-+=*/;]           	   {return yytext[0];}
.                      {ECHO; yyerror ("unexpected character");}

%%
int yywrap (void) {return 1;}