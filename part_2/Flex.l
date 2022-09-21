%option noyywrap

%{
#include <iostream>
#include <stdio.h>
#include <list>
#include <string>
#include <map>

#define YY_DECL int yylex()
#define YYSTYPE char *

#include "Bison.tab.hpp"

int lin_num = 0;
std::map<int, std::string> comments;
%}

id                      [a-zA-Z]([a-zA-Z0-9]*)
string                  [^\"]*
%%
[+-]?([0-9]*[.])?[0-9]+ yylval=strdup(yytext); return NUMBER;
true|false              yylval=strdup(yytext); return BOOL;
{id}                    yylval=strdup(yytext); return WORD;
\${id}(\.{id})?         yylval=strdup(yytext); return REFPATH;
\"{string}\"            yylval=strdup(yytext); return STRING;
\[                      return OPEN_SQUARE;
\]                      return CLOSED_SQUARE;
=                       return EQUALS;
(\r\n|\r|\n)+           ++lin_num;
[ \t]+                  /* ignore whitespace */;
\#.*\n                  comments[++lin_num] = std::string(yytext); 
%%
