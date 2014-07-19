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
typedef union {
/* Derived from $Id: qp_parser.h,v 1.3 1996/05/14 12:23:28 maartenb Exp $ */
	int	num;
	double	real;
	char	* str;
	pTree	expr;
	pTree	expr_list;
#define yylval	qp_parser_lval
} YYSTYPE;
extern YYSTYPE yylval;
