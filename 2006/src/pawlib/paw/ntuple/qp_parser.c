#ifndef lint
static char yysccsid[] = "@(#)yaccpar	1.7 (Berkeley) 09/09/90";
#endif
#define YYBYACC 1
#line 18 "qp_parser_y.y"

#include	<string.h>

#include	"qp_lexyacc.h"
#include	"mstr.h"
#include	"qp_tree.h"
#include	"qp_report.h"
#include	"str.h"

pTree	qp_the_parse_tree;	/* return the parse tree */

extern void qp_scanner_get_pos( char **, int *);

#line 31 "qp_parser_y.y"
typedef union {
/* Derived from $Id: qp_parser.c,v 1.8 1996/10/17 15:59:02 couet Exp $ */
	int	num;
	double	real;
	char	* str;
	pTree	expr;
	pTree	expr_list;
#define yylval	qp_parser_lval
} YYSTYPE;
#line 28 "y.tab.c"
#define T_UNKNOWN 257
#define T_CUT 258
#define T_IDENT 259
#define T_BOOL 260
#define T_UNSIGNED 261
#define T_INT 262
#define T_FLOAT 263
#define T_DOUBLE 264
#define T_STRING 265
#define T_PUT_MASK 266
#define T_IN 267
#define T_OR 268
#define T_AND 269
#define T_NOT 270
#define T_EQ 271
#define T_NE 272
#define T_CT 273
#define T_LT 274
#define T_LE 275
#define T_GT 276
#define T_GE 277
#define UMINUS 278
#define T_POWER 279
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,    0,    3,    3,    4,    4,    5,    5,    1,    1,
    2,    2,    6,    6,    6,    6,    6,    6,    6,    6,
    7,    7,    7,    7,    7,    7,    7,    7,    7,    7,
    7,    7,    7,    7,    7,    7,    8,    8,    8,   11,
   11,   12,   12,    9,    9,   13,   13,   14,   10,   10,
   10,   10,
};
short yylen[] = {                                         2,
    1,    6,    1,    3,    1,    3,    1,    2,    1,    1,
    1,    1,    1,    3,    5,    3,    5,    3,    3,    3,
    1,    1,    1,    1,    1,    1,    1,    1,    3,    3,
    3,    3,    2,    2,    3,    3,    1,    4,    7,    0,
    1,    1,    3,    1,    1,    1,    1,    3,    3,    2,
    2,    1,
};
short yydefred[] = {                                      0,
   27,    0,   21,   22,   23,   24,   25,   26,    0,    0,
    0,    0,    0,    0,    0,    5,    7,    0,   28,    0,
   47,    0,    8,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    9,   10,   11,   12,    0,    0,    0,    0,
    0,    0,    0,    0,   48,   36,    0,    0,    6,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   42,   44,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   43,    2,
    0,   39,
};
short yydgoto[] = {                                      13,
   42,   43,   61,   15,   16,   17,   18,   19,   63,   64,
   65,   66,   20,   21,
};
short yysindex[] = {                                    364,
    0,  -17,    0,    0,    0,    0,    0,    0,  364,  378,
  378,  364,    0, -246, -257,    0,    0,  -19,    0,  -13,
    0, -224,    0, -240, -240,  -32, -219,  364,  364,  378,
  378,  378,    0,    0,    0,    0,  378,  378,  378,  378,
  378,  378,  378,  181,    0,    0,   11, -257,    0,  116,
  116,  116,  -42,  -42, -240, -240, -240,  -11,  171,  378,
 -216,  -28,    0,    0,   12,   18,  378,  378,  378,  116,
  378,   23,  181,  -39,  116,  116,  116,  181,    0,    0,
   24,    0,
};
short yyrindex[] = {                                      0,
    0,    1,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   64,  164,    0,    0,  121,    0,   13,
    0,    0,    0,   37,   49,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   34,    0,    0,    0,  168,    0,  125,
  133,  137,   97,  109,   61,   73,   85,  144,  148,    6,
   33,  -34,    0,    0,    0,   35,    0,    0,    0,   45,
   57,   25,    0,    0,  156,  160,   69,    0,    0,    0,
    0,    0,
};
short yygindex[] = {                                      0,
   29,   40,   21,   60,    9,    0,  427,    0,  -62,    0,
    0,    0,    0,    0,
};
#define YYTABLESIZE 643
short yytable[] = {                                      39,
   46,   80,   39,   37,   40,   38,   13,   40,   46,   13,
   79,   29,   37,   39,   37,   81,   38,   23,   40,   27,
   14,   28,   39,   37,   38,   38,   44,   40,   22,   71,
   39,   37,   26,   38,   45,   40,   34,   49,   41,   47,
   46,   46,   46,   46,   46,   46,   52,   46,   33,   52,
   67,   28,   72,   37,   37,   37,   37,   37,   46,   37,
   31,   73,   78,    1,   82,   38,   38,   38,   38,   38,
   37,   38,   32,   45,   40,   41,   45,   34,   34,   34,
   34,   34,   38,   34,   35,   51,   68,   48,   51,   33,
   33,   33,   33,   33,   34,   33,   29,   50,   69,    0,
   50,   31,   31,   31,   31,   31,   33,   31,   30,   49,
    0,    0,   49,   32,   32,   32,   32,   32,   31,   32,
   13,    0,    0,    0,   18,   35,   35,   35,   35,   35,
   32,   35,   19,    0,    0,    0,   20,   29,    0,   29,
   29,   29,   35,   14,    0,    0,    0,   16,    0,   30,
    0,   30,   30,   30,   29,   15,    0,   39,   37,   17,
   38,   13,   40,    3,   13,   18,   30,    4,   18,    0,
    0,    0,    0,   19,    0,    0,   19,   20,    0,    0,
   20,    0,    0,    0,   14,    0,    0,   14,   16,    0,
    0,   16,    0,    0,    0,    0,   15,    0,    0,   15,
   17,    0,    0,   17,    3,    0,    0,    3,    4,    0,
    0,    4,   39,   37,    0,   38,    0,   40,    0,    0,
   12,    0,    0,   10,    0,   11,    0,    0,    0,    0,
    0,    0,    0,   13,   13,   28,   41,    0,   60,   41,
    0,    0,   30,   31,   32,   33,   34,   35,   36,    0,
   41,   30,   31,   32,   33,   34,   35,   36,    0,   41,
    0,    0,   33,   34,    0,    0,   46,   41,   46,   46,
    0,   46,   46,   46,   46,   46,   46,   46,   37,   46,
   37,   37,    0,   37,   37,   37,   37,   37,   37,   37,
   38,   37,   38,   38,    0,   38,   38,   38,   38,   38,
   38,   38,   34,   38,   34,   34,    0,   34,   34,   34,
   34,   34,   34,   34,   33,    0,   33,   33,    0,   33,
   33,   33,   33,   33,   33,   33,   31,    0,   31,   31,
    0,   31,   31,   31,   31,   31,   31,   31,   32,    0,
   32,   32,    0,   32,   32,   32,   32,   32,   32,   32,
   35,    0,   35,   35,    0,   35,   35,   35,   35,   35,
   35,   35,   29,    0,   29,   29,    0,   29,   29,   29,
   29,   29,   29,   29,   30,    0,   30,   30,    0,   30,
   30,   30,   30,   30,   30,   30,   13,    0,   13,   13,
   18,    0,   18,   18,   41,    0,    0,    0,   19,    0,
   19,   19,   20,   12,   20,   20,   10,    0,   11,   14,
    0,   14,   14,   16,    0,   16,   16,   12,    0,    0,
   10,   15,   11,   15,   15,   17,    0,   17,   17,    3,
    0,    3,    0,    4,    0,    4,   24,   25,    1,    2,
    3,    4,    5,    6,    7,    8,   35,   36,    0,   41,
    9,    0,    0,    0,    0,    0,   50,   51,   52,    0,
    0,    0,    0,   53,   54,   55,   56,   57,   58,   59,
   62,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   70,    0,    0,    0,
    0,    0,    0,   74,   75,   76,    0,   77,    0,   62,
    0,    0,    0,    0,   62,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    1,    2,    3,    4,    5,    6,    7,    8,    0,
    0,    0,    0,    9,    0,    1,    2,    3,    4,    5,
    6,    7,    8,
};
short yycheck[] = {                                      42,
    0,   41,   42,   43,   47,   45,   41,   47,   41,   44,
   73,  269,    0,   42,   43,   78,   45,    9,   47,  266,
    0,  268,   42,   43,    0,   45,   40,   47,   46,   58,
   42,   43,   12,   45,  259,   47,    0,   29,  279,  259,
   40,   41,   42,   43,   44,   45,   41,   47,    0,   44,
   40,  268,   41,   41,   42,   43,   44,   45,   58,   47,
    0,   44,   40,    0,   41,   41,   42,   43,   44,   45,
   58,   47,    0,   41,   41,   41,   44,   41,   42,   43,
   44,   45,   58,   47,    0,   41,   58,   28,   44,   41,
   42,   43,   44,   45,   58,   47,    0,   41,   59,   -1,
   44,   41,   42,   43,   44,   45,   58,   47,    0,   41,
   -1,   -1,   44,   41,   42,   43,   44,   45,   58,   47,
    0,   -1,   -1,   -1,    0,   41,   42,   43,   44,   45,
   58,   47,    0,   -1,   -1,   -1,    0,   41,   -1,   43,
   44,   45,   58,    0,   -1,   -1,   -1,    0,   -1,   41,
   -1,   43,   44,   45,   58,    0,   -1,   42,   43,    0,
   45,   41,   47,    0,   44,   41,   58,    0,   44,   -1,
   -1,   -1,   -1,   41,   -1,   -1,   44,   41,   -1,   -1,
   44,   -1,   -1,   -1,   41,   -1,   -1,   44,   41,   -1,
   -1,   44,   -1,   -1,   -1,   -1,   41,   -1,   -1,   44,
   41,   -1,   -1,   44,   41,   -1,   -1,   44,   41,   -1,
   -1,   44,   42,   43,   -1,   45,   -1,   47,   -1,   -1,
   40,   -1,   -1,   43,   -1,   45,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  268,  269,  268,  279,   -1,   58,  279,
   -1,   -1,  271,  272,  273,  274,  275,  276,  277,   -1,
  279,  271,  272,  273,  274,  275,  276,  277,   -1,  279,
   -1,   -1,  274,  275,   -1,   -1,  266,  279,  268,  269,
   -1,  271,  272,  273,  274,  275,  276,  277,  266,  279,
  268,  269,   -1,  271,  272,  273,  274,  275,  276,  277,
  266,  279,  268,  269,   -1,  271,  272,  273,  274,  275,
  276,  277,  266,  279,  268,  269,   -1,  271,  272,  273,
  274,  275,  276,  277,  266,   -1,  268,  269,   -1,  271,
  272,  273,  274,  275,  276,  277,  266,   -1,  268,  269,
   -1,  271,  272,  273,  274,  275,  276,  277,  266,   -1,
  268,  269,   -1,  271,  272,  273,  274,  275,  276,  277,
  266,   -1,  268,  269,   -1,  271,  272,  273,  274,  275,
  276,  277,  266,   -1,  268,  269,   -1,  271,  272,  273,
  274,  275,  276,  277,  266,   -1,  268,  269,   -1,  271,
  272,  273,  274,  275,  276,  277,  266,   -1,  268,  269,
  266,   -1,  268,  269,  279,   -1,   -1,   -1,  266,   -1,
  268,  269,  266,   40,  268,  269,   43,   -1,   45,  266,
   -1,  268,  269,  266,   -1,  268,  269,   40,   -1,   -1,
   43,  266,   45,  268,  269,  266,   -1,  268,  269,  266,
   -1,  268,   -1,  266,   -1,  268,   10,   11,  258,  259,
  260,  261,  262,  263,  264,  265,  276,  277,   -1,  279,
  270,   -1,   -1,   -1,   -1,   -1,   30,   31,   32,   -1,
   -1,   -1,   -1,   37,   38,   39,   40,   41,   42,   43,
   44,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   60,   -1,   -1,   -1,
   -1,   -1,   -1,   67,   68,   69,   -1,   71,   -1,   73,
   -1,   -1,   -1,   -1,   78,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  258,  259,  260,  261,  262,  263,  264,  265,   -1,
   -1,   -1,   -1,  270,   -1,  258,  259,  260,  261,  262,
  263,  264,  265,
};
#define YYFINAL 13
#ifndef YYDEBUG
#define YYDEBUG 1
#endif
#define YYMAXTOKEN 279
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,"'('","')'","'*'","'+'","','","'-'","'.'","'/'",0,0,0,0,0,0,0,0,0,0,
"':'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,"T_UNKNOWN","T_CUT","T_IDENT","T_BOOL","T_UNSIGNED","T_INT","T_FLOAT",
"T_DOUBLE","T_STRING","T_PUT_MASK","T_IN","T_OR","T_AND","T_NOT","T_EQ","T_NE",
"T_CT","T_LT","T_LE","T_GT","T_GE","UMINUS","T_POWER",
};
char *yyrule[] = {
"$accept : line",
"line : bool_expr",
"line : bool_expr T_PUT_MASK T_IDENT '(' expression ')'",
"bool_expr : bool_term",
"bool_expr : bool_expr T_OR bool_term",
"bool_term : bool_fact",
"bool_term : bool_term T_AND bool_fact",
"bool_fact : comparison",
"bool_fact : T_NOT bool_fact",
"lte_op : T_LT",
"lte_op : T_LE",
"gte_op : T_GT",
"gte_op : T_GE",
"comparison : expression",
"comparison : expression lte_op expression",
"comparison : expression lte_op expression lte_op expression",
"comparison : expression gte_op expression",
"comparison : expression gte_op expression gte_op expression",
"comparison : expression T_EQ expression",
"comparison : expression T_NE expression",
"comparison : expression T_CT expression",
"expression : T_BOOL",
"expression : T_UNSIGNED",
"expression : T_INT",
"expression : T_FLOAT",
"expression : T_DOUBLE",
"expression : T_STRING",
"expression : T_CUT",
"expression : function_array",
"expression : expression '+' expression",
"expression : expression '-' expression",
"expression : expression '*' expression",
"expression : expression '/' expression",
"expression : '-' expression",
"expression : '+' expression",
"expression : expression T_POWER expression",
"expression : '(' bool_expr ')'",
"function_array : name",
"function_array : name '(' arg_list ')'",
"function_array : name '(' arg_list ')' '(' argument ')'",
"arg_list :",
"arg_list : one_or_more_args",
"one_or_more_args : argument",
"one_or_more_args : one_or_more_args ',' argument",
"argument : range",
"argument : bool_expr",
"name : T_IDENT",
"name : simple_file",
"simple_file : T_IDENT '.' T_IDENT",
"range : expression ':' expression",
"range : expression ':'",
"range : ':' expression",
"range : ':'",
};
#endif
#define yyclearin (yychar=(-1))
#define yyerrok (yyerrflag=0)
#ifdef YYSTACKSIZE
#ifndef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
#endif
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 600
#define YYMAXDEPTH 600
#endif
#endif
int yydebug;
int yynerrs;
int yyerrflag;
int yychar;
short *yyssp;
YYSTYPE *yyvsp;
YYSTYPE yyval;
YYSTYPE yylval;
short yyss[YYSTACKSIZE];
YYSTYPE yyvs[YYSTACKSIZE];
#define yystacksize YYSTACKSIZE
#line 343 "qp_parser_y.y"

int
qp_scanner_wrap( void )
{
	return 1;
}

void
qp_parser_error( char *message )
{
	char	*s;
	int	index;

	sf_report( " %s\n", message );
	qp_scanner_get_pos( &s, &index );
	if ( index != -1) {
		sf_report( " %s\n", s );
		s = str_new( s );
		memset( s, ' ', strlen( s ) );
		s[index>0 ? index-1 : 0] = '^';
		sf_report( " %s\n", s );
		str_del( s );
	}
}
#line 369 "y.tab.c"
#define YYABORT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab
int
yyparse()
{
    register int yym, yyn, yystate;
#if YYDEBUG
    register char *yys;
    extern char *getenv();

    if (yys = getenv("YYDEBUG"))
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = (-1);

    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

yyloop:
    if (yyn = yydefred[yystate]) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("yydebug: state %d, reading %d (%s)\n", yystate,
                    yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("yydebug: state %d, shifting to state %d\n",
                    yystate, yytable[yyn]);
#endif
        if (yyssp >= yyss + yystacksize - 1)
        {
            goto yyoverflow;
        }
        *++yyssp = yystate = yytable[yyn];
        *++yyvsp = yylval;
        yychar = (-1);
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;
#ifdef lint
    goto yynewerror;
#endif
yynewerror:
    yyerror("syntax error");
#ifdef lint
    goto yyerrlab;
#endif
yyerrlab:
    ++yynerrs;
yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yyssp]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("yydebug: state %d, error recovery shifting\
 to state %d\n", *yyssp, yytable[yyn]);
#endif
                if (yyssp >= yyss + yystacksize - 1)
                {
                    goto yyoverflow;
                }
                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("yydebug: error recovery discarding state %d\n",
                            *yyssp);
#endif
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("yydebug: state %d, error recovery discards token %d (%s)\n",
                    yystate, yychar, yys);
        }
#endif
        yychar = (-1);
        goto yyloop;
    }
yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("yydebug: state %d, reducing by rule %d (%s)\n",
                yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    yyval = yyvsp[1-yym];
    switch (yyn)
    {
case 1:
#line 81 "qp_parser_y.y"
{
				qp_the_parse_tree =  yyvsp[0].expr;
			}
break;
case 2:
#line 86 "qp_parser_y.y"
{
				qp_the_parse_tree = new_mask_node( yyvsp[-5].expr, yyvsp[-3].str, yyvsp[-1].expr );
			}
break;
case 4:
#line 95 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_OR, yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 6:
#line 101 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_AND, yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 8:
#line 107 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_NOT, yyvsp[0].expr, 0, 0 ); }
break;
case 9:
#line 112 "qp_parser_y.y"
{ yyval.num = O_LT; }
break;
case 10:
#line 114 "qp_parser_y.y"
{ yyval.num = O_LE; }
break;
case 11:
#line 119 "qp_parser_y.y"
{ yyval.num = O_GT; }
break;
case 12:
#line 121 "qp_parser_y.y"
{ yyval.num = O_GE; }
break;
case 14:
#line 127 "qp_parser_y.y"
{ yyval.expr = new_op_node( yyvsp[-1].num , yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 15:
#line 129 "qp_parser_y.y"
{
				pTree	c1, c2;
				OpType	op;

				if ( yyvsp[-3].num == O_LT ) {
					if ( yyvsp[-1].num == O_LT ) 	op = O_LTLT;
					else 			op = O_LTLE;
				} else {
					if ( yyvsp[-1].num == O_LT ) 	op = O_LELT;
					else 			op = O_LELE;
				}


				yyval.expr = new_op_node( op, yyvsp[-4].expr, yyvsp[-2].expr, yyvsp[0].expr );
			}
break;
case 16:
#line 145 "qp_parser_y.y"
{ yyval.expr = new_op_node( yyvsp[-1].num, yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 17:
#line 147 "qp_parser_y.y"
{
				pTree	c1, c2;
				OpType	op;

				if ( yyvsp[-3].num == O_GT ) {
					if ( yyvsp[-1].num == O_GT ) 	op = O_GTGT;
					else 			op = O_GTGE;
				} else {
					if ( yyvsp[-1].num == O_GT ) 	op = O_GEGT;
					else 			op = O_GEGE;
				}


				yyval.expr = new_op_node( op, yyvsp[-4].expr, yyvsp[-2].expr, yyvsp[0].expr );
			}
break;
case 18:
#line 163 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_EQ, yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 19:
#line 165 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_NE, yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 20:
#line 167 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_CT, yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 21:
#line 172 "qp_parser_y.y"
{ yyval.expr = new_con_bool_node( yyvsp[0].num ); }
break;
case 22:
#line 174 "qp_parser_y.y"
{ yyval.expr = new_con_uint_node( yyvsp[0].num ); }
break;
case 23:
#line 176 "qp_parser_y.y"
{ yyval.expr = new_con_int_node( yyvsp[0].num ); }
break;
case 24:
#line 178 "qp_parser_y.y"
{ yyval.expr = new_con_float_node( yyvsp[0].real ); }
break;
case 25:
#line 180 "qp_parser_y.y"
{ yyval.expr = new_con_double_node( yyvsp[0].real ); }
break;
case 26:
#line 182 "qp_parser_y.y"
{
				yyval.expr = new_con_str_node( yyvsp[0].str );
				mstr_del( yyvsp[0].str );
			}
break;
case 27:
#line 187 "qp_parser_y.y"
{ yyval.expr = new_cut_node( yyvsp[0].num ); }
break;
case 29:
#line 191 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_PLUS, yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 30:
#line 193 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_MINUS, yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 31:
#line 195 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_TIMES, yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 32:
#line 197 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_DIV, yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 33:
#line 199 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_UMINUS, yyvsp[0].expr, 0, 0 ); }
break;
case 34:
#line 201 "qp_parser_y.y"
{ yyval.expr = yyvsp[0].expr; }
break;
case 35:
#line 203 "qp_parser_y.y"
{ yyval.expr = new_op_node( O_POW, yyvsp[-2].expr, yyvsp[0].expr, 0 ); }
break;
case 36:
#line 205 "qp_parser_y.y"
{ yyval.expr = yyvsp[-1].expr; }
break;
case 37:
#line 210 "qp_parser_y.y"
{ 
				mstr_release( yyvsp[0].str );
				yyval.expr = new_name_node( yyvsp[0].str, 0, 0 );
			}
break;
case 38:
#line 215 "qp_parser_y.y"
{
				mstr_release( yyvsp[-3].str );
				yyval.expr = new_name_node( yyvsp[-3].str, yyvsp[-1].expr_list, 0 );
			}
break;
case 39:
#line 220 "qp_parser_y.y"
{
				mstr_release( yyvsp[-6].str );
				yyval.expr = new_name_node( yyvsp[-6].str, yyvsp[-4].expr_list, yyvsp[-1].expr );
			}
break;
case 40:
#line 228 "qp_parser_y.y"
{ yyval.expr_list = 0; }
break;
case 42:
#line 234 "qp_parser_y.y"
{ yyvsp[0].expr->next = 0; yyval.expr_list = yyvsp[0].expr; }
break;
case 43:
#line 236 "qp_parser_y.y"
{ yyvsp[0].expr->next = yyvsp[-2].expr_list; yyval.expr_list = yyvsp[0].expr; }
break;
case 48:
#line 257 "qp_parser_y.y"
{ yyval.str = mstr_merge( yyvsp[-2].str, mstr_new("."), yyvsp[0].str, (char*) 0 ); }
break;
case 49:
#line 320 "qp_parser_y.y"
{ yyval.expr = new_range_node( yyvsp[-2].expr, yyvsp[0].expr ); }
break;
case 50:
#line 322 "qp_parser_y.y"
{ yyval.expr = new_range_node( yyvsp[-1].expr, (pTree) 0 ); }
break;
case 51:
#line 324 "qp_parser_y.y"
{ yyval.expr = new_range_node( (pTree) 0, yyvsp[0].expr ); }
break;
case 52:
#line 326 "qp_parser_y.y"
{ yyval.expr = new_range_node( (pTree) 0, (pTree) 0 ); }
break;
#line 721 "y.tab.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("yydebug: after reduction, shifting from state 0 to\
 state %d\n", YYFINAL);
#endif
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("yydebug: state %d, reading %d (%s)\n",
                        YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("yydebug: after reduction, shifting from state %d \
to state %d\n", *yyssp, yystate);
#endif
    if (yyssp >= yyss + yystacksize - 1)
    {
        goto yyoverflow;
    }
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;
yyoverflow:
    yyerror("yacc stack overflow");
yyabort:
    return (1);
yyaccept:
    return (0);
}
