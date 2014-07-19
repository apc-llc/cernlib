/*
 *  qp_scanner.l  - (flex) scanner source for selfun
 *
 *  Original: 15-Jun-1994 15:16
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_scanner_l.l,v 1.10 1996/05/14 12:23:31 maartenb Exp $
 *
 *  $Log: qp_scanner_l.l,v $
 *  Revision 1.10  1996/05/14 12:23:31  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.9  1996/04/23 18:38:57  maartenb
 *  - Add RCS keywords
 *
 *
 */

%{
/* Derived from $Id: qp_scanner_l.l,v 1.10 1996/05/14 12:23:31 maartenb Exp $ */

#define		YY_NEVER_INTERACTIVE	1

#include	<ctype.h>
#include	<math.h>
#include	<stdlib.h>
#include	<string.h>

#include	"str.h"
#include	"mstr.h"
#include	"qp_lexyacc.h"
#include	"qp_report.h"
#include	"qp_tree.h"

#include	"qp_parser.h" /* generated, should be included last */

#ifdef LEX_DEBUG
#define DB { printf( "\"%s\"\n", yytext ); }
#define DB1(ID) { printf( "%s: \"%s\"\n", ID, yytext ); }
#else
#define DB
#define DB1(ID)
#endif


static int	inp_len = 0;
static int	inp_index = 0;
static char	*inp_buf = 0;

void
qp_scanner_set_input( char *s )
{
	if ( inp_buf ) free( inp_buf );

	inp_buf = str_new( s );
	inp_len = strlen( s );
	inp_index = 0;

	yyrestart( stdin ); /* stdin is just ref'd for isatty :-( */
}


void
qp_scanner_get_pos( char **s, int *i )
{
	if ( inp_buf ) {
		*s = inp_buf;
		*i = inp_index;
	} else {
		*s = 0;
		*i = -1;
	}
}


/*
#define YY_INPUT( buf, result, max_size ) { \
	int len = max_size > inp_len ? inp_len : max_size; \
	memcpy( buf, &inp_buf[inp_index], len ); \
	inp_index += len; inp_len -= len; \
	result = len; }
*/
	
yy_input_routine( char *buf, int * result, int max_size )
{
/*
	int len = max_size > inp_len ? inp_len : max_size;
	memcpy( buf, &inp_buf[inp_index], len );
	inp_index += len;
	inp_len -= len;
	*result = len;
*/
	if ( inp_len > 0 ) {
		buf[0] = inp_buf[inp_index];
		inp_index += 1;
		inp_len -= 1;
		*result = 1;
	} else {
		*result = YY_NULL;
	}
}

#define YY_INPUT( buf, result, max_size ) \
	yy_input_routine( buf, &(result), max_size )

static int
int_conv( char	* const str )
{
	return atoi( str );
}


static int
uint_conv( char	*str )
{
	int	r, base, val;

	switch ( *str ) {
	case 'z': case 'Z':	base = 16; break;
	case 'o': case 'O':	base = 8; break;
	case 'b': case 'B':	base = 2; break;
	}

	for ( r=0, str += 2; *str != '\''; str++ ) {
		if ( isdigit( *str ) )
			r = r * base + ( *str - '0' );
		else if ( isupper( *str ) )
			r = r * base + ( *str - 'A' + 10 );
		else /* islower( *str ) */
			r = r * base + ( *str - 'a' + 10 );
	}

	return r;
}


static double
double_conv( char * const str )
{
	double	lfval;
	char	*p, *end, save;

	p = strchr( str, 'd' );

	if ( p != 0 ) {
		save = *p;
		*p = 'e';
	} else {
		p = strchr( str, 'D' );
		if ( p != 0 ) {
			save = *p;
			*p = 'e';
		}
	}

	lfval = strtod( str, & end );

	if ( *end != '\0' ) {	/* did not convert the full string ?? */
		qp_abort( "double_conv: strtod failed\n" );
	}

	if ( p != 0 ) {
		*p = save;
	}

	return lfval;
}


%}

OR	\.[Oo][Rr]\.
AND	\.[Aa][Nn][Dd]\.
NOT	\.[Nn][Oo][Tt]\.

LT	\.[Ll][Tt]\.
LE	\.[Ll][Ee]\.
GT	\.[Gg][Tt]\.
GE	\.[Gg][Ee]\.

EQ	\.[Ee][Qq]\.
NE	\.[Nn][Ee]\.
CT	\.[Cc][Tt]\.

TRUE	\.[Tt][Rr][Uu][Ee]\.
FALSE	\.[Ff][Aa][Ll][Ss][Ee]\.

LET	[a-zA-Z_$]
NUMLET	[a-zA-Z0-9_$]
INT	[0-9]+
EXP	([eE][+-]?{INT})
DEXP	([dD][+-]?{INT})
MANT	(({INT}"."?)|({INT}?"."{INT}))
IN	\.[Ii][Nn]\.

%%

">>"		{ DB1("T_PUT_MASK"); return T_PUT_MASK;}

{IN}		{ DB1("T_IN"); return T_IN;}

"||"|{OR}	{ DB1("T_OR"); return T_OR;}
"&&"|{AND}	{ DB1("T_AND"); return T_AND;}
"!"|{NOT}	{ DB1("T_NOT"); return T_NOT;}

"<"|{LT}	{ DB1("T_LT"); return T_LT;}
"<="|{LE}	{ DB1("T_LE"); return T_LE;}

">"|{GT}	{ DB1("T_GT"); return T_GT;}
">="|{GE}	{ DB1("T_GE"); return T_GE;}

"=="|"="|{EQ}	{ DB1("T_EQ"); return T_EQ;}
"!="|"<>"|{NE}	{ DB1("T_NE"); return T_NE;}
"#"|{CT}	{ DB1("T_NE"); return T_NE;}

"**"|"^"	{ DB1("T_POWER"); return T_POWER;}

"$"{INT}	{ DB1("T_CUT"); qp_parser_lval.num = int_conv(&yytext[1]); return T_CUT;}

{TRUE}		{ DB1("T_BOOL"); qp_parser_lval.num = 1; return T_BOOL;}
{FALSE}		{ DB1("T_BOOL"); qp_parser_lval.num = 0; return T_BOOL;}

[Zz]\'[a-fA-F0-9]+\'	|
[Oo]\'[0-7]+\'		|
[Bb]\'[01]+\'	{ DB1("T_UNSIGNED"); qp_parser_lval.num = uint_conv(yytext); return T_UNSIGNED;}

{LET}{NUMLET}*	{ DB1("T_IDENT"); qp_parser_lval.str = mstr_new(yytext); return T_IDENT;}

{INT}/{OR}	{ DB1("T_INT"); qp_parser_lval.num = int_conv(yytext); return T_INT;}
{INT}/{AND}	{ DB1("T_INT"); qp_parser_lval.num = int_conv(yytext); return T_INT;}
{INT}/{EQ}	{ DB1("T_INT"); qp_parser_lval.num = int_conv(yytext); return T_INT;}
{INT}/{NE}	{ DB1("T_INT"); qp_parser_lval.num = int_conv(yytext); return T_INT;}
{INT}/{LT}	{ DB1("T_INT"); qp_parser_lval.num = int_conv(yytext); return T_INT;}
{INT}/{LE}	{ DB1("T_INT"); qp_parser_lval.num = int_conv(yytext); return T_INT;}
{INT}/{GT}	{ DB1("T_INT"); qp_parser_lval.num = int_conv(yytext); return T_INT;}
{INT}/{GE}	{ DB1("T_INT"); qp_parser_lval.num = int_conv(yytext); return T_INT;}
{INT}/{CT}	{ DB1("T_INT"); qp_parser_lval.num = int_conv(yytext); return T_INT;}
{INT}		{ DB1("T_INT"); qp_parser_lval.num = int_conv(yytext); return T_INT;}

{MANT}{EXP}?	{ DB1("T_FLOAT"); qp_parser_lval.real = double_conv(yytext); return T_FLOAT;}

{MANT}{DEXP}?	{ DB1("T_DOUBLE"); qp_parser_lval.real = double_conv(yytext); return T_DOUBLE;}

\'[^'\n]*\'	{ DB1("T_STRING"); qp_parser_lval.str = mstr_new(&yytext[1]);
			qp_parser_lval.str[strlen(yytext)-2] = 0; return T_STRING;}

\"[^"\n]*\"	{ DB1("T_IDENT"); qp_parser_lval.str = mstr_new(&yytext[1]);
			qp_parser_lval.str[strlen(yytext)-2] = 0; return T_IDENT;}

[*/+\-$[\].():,~]	{ DB; return *yytext; }

\n		yyterminate();
<<EOF>>		yyterminate();

.		{ DB1("T_UNKNOWN"); return T_UNKNOWN;}
