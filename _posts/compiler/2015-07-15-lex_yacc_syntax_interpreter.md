---
title: 用Lex和Yacc实现解释器
author: Tao He
date: 2015-07-15
tags: [Compiler]
category: 编译原理
layout: post
---

语法树是句子结构的图形表示，它代表了句子的推导结果，有利于理解句子语法结构的层次。简单说，语法树就是按照某一规则进行推导时所形成的树。不论什么语言，语法结构总是那几种，可以想象任何程序体都可以解释成一棵语法树，语法树的本质是递归，很显然Yacc文法的核心思想也是递归。

**一棵语法树包括了一个句型的所有可能的推导过程。**通过Lex，Yacc和语法树的结合，可以很方便地来实现一个微型的解释器程序(interpreter)。

<!--more-->

解释器功能
--------

支持赋值功能：`=`。

能实现四则运算，布尔运算，`if`语句判断，`while`循环。另外，还可以实现注释功能(`/* ... */`)和打印。

四则运算包括： 加(`+`)，减(`—`)，乘(`*`)，除(`/`)，求余(`%`)

布尔运算包括: AND(`&&`), OR(`||`), NOT(`!`)

比较运算包括：GE(`>=`), LE(`<=`), GT(`>`), LT(`<`), NE(`!=`或`<>`), EQ(`==`)。

IF-ELSE语句： 支持以下几种语法：

~~~c
if(cond) stmt

if(cond) stmt
else stmt

if(cond) { stmt }
else { stmt }
~~~

WHILE语句：支持以下几种语法：

~~~c
while(cond) stmt

while(cond) { stmt }
~~~

支持变量，但变量名必须是单个小写字母(`[a-z]`)。

支持PRINT，语法为：

~~~c
print a;

print 100;
~~~

所有的值必须都为**整数**。

语法树节点定义
------------

~~~c
/* Node type definition. */
typedef enum {
    TYPE_CONTENT, TYPE_INDEX, TYPE_OP
} NodeEnum;

/* Operators. */
typedef struct {
    int name;
    int num;
    // we need two child nodes under almost all cases. for avoiding compile error,
    // so the length of *node array should no shorter than 2.
    struct NodeTag *node[1];
} OpNode;

/* Node */
typedef struct NodeTag {
    NodeEnum type;
    // for different type of node, there will be different type of value.
    union {
        int content;
        int index;
        OpNode op;
    };
} Node;

// simulate memory to storage the value of 26 variables([a-z])
extern int memory[26];
~~~

Lex代码
------

~~~c
%{
#include <stdlib.h>
#include "node.h"
#include "y.tab.h"

void yyerror(char *);
%}

%%

"/*"([^\*]|(\*)*[^\*/])*(\*)*"*/" {} // multi-line comments.

/* control-flow keywords */
"while" { return WHILE; }
"if" { return IF; }
"else" { return ELSE; }
"print" { return PRINT; }
/* boolean constant value. */
"false" { yylval.ivalue = 0; return INTEGER; }
"true" { yylval.ivalue = 1; return INTEGER; }
[a-z] { yylval.sindex = *yytext - 'a'; return VARIABLE; }
[0-9]+ { yylval.ivalue = atoi(yytext); return INTEGER; }
[\+\-\*\/\(\)\%;{}=] { return *yytext; }
">=" { return GE; }
"<=" { return LE; }
">" { return GT; }
"<" { return LT; }
"==" { return EQ; }
"!=" { return NE; }
"<>" { return NE; }
"&&" { return AND; }
"\|\|" { return OR; }
"!" { return NOT; }
[\t\n ]+ {} /* skip all whitespace, space, \t and \n */
. { printf("unknown symbol, char: %c, ascii: %d\n", *yytext, (int)*yytext); }

%%

int yywarp()
{
    return 1;
}
~~~

Yacc代码
--------

~~~c
%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "node.h"

Node *opr(int name, int num, ...);

Node *set_index(int value);
Node *set_content(int value);

void freeNode(Node *p);
int execNode(Node *p);
int yylexeNode();

void yyerror(char *s);

int memory[26];
%}

%union {
    int ivalue; // variable's value.
    int sindex; // variable array's index.
    Node *nptr; // node address.
};

%token <ivalue> VARIABLE
%token <sindex> INTEGER
%token WHILE IF PRINT

%nonassoc IFX
%nonassoc ELSE

%left AND OR GE LE EQ NE GT LT
%left '+' '-'
%left '*' '/' '%'

%nonassoc UMINUS NOT
%type <nptr> stmt expr stmt_list

%%

program:
    function { exit(0); }
    ;

function:
    function stmt { execNode($2); freeNode($2); }
    | /* null statement. */
    ;

stmt:
    ';' { $$ = opr(';', 2, NULL, NULL); }
    | expr ';' { $$ = $1; }
    | PRINT expr ';' { $$ = opr(PRINT, 1, $2); }
    | VARIABLE '=' expr ';' { $$ = opr('=', 2, set_index($1), $3); }
    | WHILE '(' expr ')' stmt { $$ = opr(WHILE, 2, $3, $5); }
    | IF '(' expr ')' stmt %prec IFX { $$ = opr(IF, 2, $3, $5); }
    | IF '(' expr ')' stmt ELSE stmt %prec ELSE { $$ = opr(IF, 3, $3, $5, $7); }
    | '{' stmt_list '}' { $$ = $2; }
    ;

stmt_list:
    stmt { $$ = $1; }
    | stmt_list stmt { $$ = opr(';', 2, $2, $1); }
    ;

expr:
    INTEGER { $$ = set_content($1); }
    | VARIABLE { $$ = set_index($1); }
    | expr '+' expr { $$ = opr('+', 2, $1, $3); }
    | expr '-' expr { $$ = opr('-', 2, $1, $3); }
    | expr '*' expr { $$ = opr('*', 2, $1, $3); }
    | expr '/' expr { $$ = opr('/', 2, $1, $3); }
    | expr '%' expr { $$ = opr('%', 2, $1, $3); }
    | expr GT expr { $$ = opr(GT, 2, $1, $3); }
    | expr GE expr { $$ = opr(GE, 2, $1, $3); }
    | expr LE expr { $$ = opr(LE, 2, $1, $3); }
    | expr LT expr { $$ = opr(LT, 2, $1, $3); }
    | expr NE expr { $$ = opr(NE, 2, $1, $3); }
    | expr EQ expr { $$ = opr(EQ, 2, $1, $3); }
    | expr AND expr { $$ = opr(AND, 2, $1, $3); }
    | expr OR expr { $$ = opr(OR, 2, $1, $3); }
    | NOT expr %prec NOT { $$ = opr(NOT, 1, $2); }
    | '-' expr %prec UMINUS { $$ = opr(UMINUS, 1, $2); }
    | '(' expr ')' { $$ = $2; }
    ;

%%

#define SIZE_OF_HEAD ((char *)&p->content-(char *)p)

Node *set_content(int value)
{
    Node *p;
    size_t nsize = SIZE_OF_HEAD + sizeof(int);
    if((p = malloc(nsize)) == NULL) {
        yyerror("out of memory @ set_content.\n");
    }
    p->type = TYPE_CONTENT;
    p->content = value;

    return p;
}

Node *set_index(int value)
{
    Node *p;
    size_t nsize = SIZE_OF_HEAD + sizeof(int);
    if((p = malloc(nsize)) == NULL) {
        yyerror("out of memory @ set_index.\n");
    }
    p->type = TYPE_INDEX;
    p->index = value;

    return p;
}

Node *opr(int name, int num, ...)
{
    va_list ap;
    Node *p;
    size_t nsize = SIZE_OF_HEAD + sizeof(OpNode) + num * sizeof(Node *);
    if((p = malloc(nsize)) == NULL) {
        yyerror("out of memory @ opr.\n");
    }
    p->type = TYPE_OP;
    p->op.name = name;
    p->op.num = num;
    va_start(ap, num);
    int i = 0;
    for(i = 0; i < num; ++i) {
        p->op.node[i] = va_arg(ap, Node *);
    }
    va_end(ap);
    p->op.node[num] = NULL;     // set the last node ptr as NULL.
    return p;
}

/**
 * calculate the value of the node in syntax tree.
 * this is the core method to eval the syntax tree.
 */
int execNode(Node *p)
{
    if(!p) { return 0; }
    switch (p->type) {
    case TYPE_CONTENT: return p->content;
    case TYPE_INDEX: return memory[p->index];
    case TYPE_OP:
        switch(p->op.name) {
        case WHILE:
            while(execNode(p->op.node[0])) {
                execNode(p->op.node[1]);
            }
            return 0;
        case IF:
            if(execNode(p->op.node[0])) {
                execNode(p->op.node[1]);
            }
            else if(p->op.num > 2) {
                execNode(p->op.node[2]);
            }
            return 0;
        case PRINT:
            printf("%d\n", execNode(p->op.node[0]));
            return 0;
        case ';':
            execNode(p->op.node[0]);
            return execNode(p->op.node[1]);
        case '=':
            return memory[p->op.node[0]->index] = execNode(p->op.node[1]);
        case UMINUS:
            return -execNode(p->op.node[0]);
        case '+':
            return execNode(p->op.node[0]) + execNode(p->op.node[1]);
        case '-':
            return execNode(p->op.node[0]) - execNode(p->op.node[1]);
        case '*':
            return execNode(p->op.node[0]) * execNode(p->op.node[1]);
        case '/':
            return execNode(p->op.node[0]) / execNode(p->op.node[1]);
        case GE:
            return execNode(p->op.node[0]) >= execNode(p->op.node[1]);
        case GT:
            return execNode(p->op.node[0]) > execNode(p->op.node[1]);
        case LE:
            return execNode(p->op.node[0]) <= execNode(p->op.node[1]);
        case LT:
            return execNode(p->op.node[0]) < execNode(p->op.node[1]);
        case AND:
            return execNode(p->op.node[0]) && execNode(p->op.node[1]);
        case OR:
            return execNode(p->op.node[0]) || execNode(p->op.node[1]);
        case NOT:
            return !execNode(p->op.node[0]);
        case EQ:
            return execNode(p->op.node[0]) == execNode(p->op.node[1]);
        case NE:
            return execNode(p->op.node[0]) != execNode(p->op.node[1]);
        }
    }

    return 0;
}

void freeNode(Node *p)
{
    if(!p) { return; }
    if(p->type == TYPE_OP) {
        int i = 0;
        for(i = 0; i < p->op.num; ++i) {
            freeNode(p->op.node[i]); // free child node.
        }
    }
    free(p);
}

void yyerror(char *s)
{
    printf("ERROR: %s\n", s);
}

int main(int argc, char **argv)
{
    yyparse();

    return 0;
}
~~~

构建脚本Makefile
---------------

~~~Makefile
##
# Copyright: He Tao, sighingnow@outlook.com
# 2015-07-14
##

all: syntaxtree

syntaxtree: lex.yy.c y.tab.c
    cc lex.yy.c y.tab.c -o syntaxtree -ll -ly -I./ -std=c11

lex.yy.c: syntaxtree.lex
    lex syntaxtree.lex

y.tab.c:
    yacc syntaxtree.y -d

clean:
    rm lex.yy.c -f
    rm y.tab.c -f
    rm y.tab.h -f
    rm syntaxtree -f
.PHONY: clean
~~~

例程
----

~~~
a = 100;
while(a) { a = a / 2; print a; }
~~~

源代码下载： [Syntax-Tree.tar.gz]({{site.url}}/resource/lex_yacc_interpreter/Syntax-Tree.tar.gz)
