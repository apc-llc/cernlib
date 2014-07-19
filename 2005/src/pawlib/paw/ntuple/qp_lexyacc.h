/*
 *  qp_lexyacc.h  --
 *	Define all (b)yacc and (f)lex external symbols to
 *	have qp_{scanner,parser}_ i.s.o yy as leading char's
 *
 *  Original: 14-Dec-1994 11:27
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_lexyacc.h,v 1.2 1996/04/23 18:38:44 maartenb Exp $
 *
 *  $Log: qp_lexyacc.h,v $
 *  Revision 1.2  1996/04/23 18:38:44  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_LEXYACC
#define CERN_QP_LEXYACC

#define yydebug		qp_parser_debug
#define yynerrs		qp_parser_nerrs
#define yyerrflag	qp_parser_errflag
#define yychar		qp_parser_char
#define yyssp		qp_parser_ssp
#define yyvsp		qp_parser_vsp
#define yyval		qp_parser_val
#define yyss		qp_parser_ss
#define yyvs		qp_parser_vs
#define	yylhs		qp_parser_lhs
#define	yylen		qp_parser_len
#define	yydefred	qp_parser_defred
#define	yydgoto		qp_parser_dgoto
#define	yysindex	qp_parser_sindex
#define	yyrindex	qp_parser_rindex
#define	yygindex	qp_parser_gindex
#define	yytable		qp_parser_table
#define	yycheck		qp_parser_check
#define	yyname		qp_parser_name
#define	yyrule		qp_parser_rule
#define	yyparse		qp_parser_parse
#define yyerror		qp_parser_error

#ifndef yylex		/* only in parser */

#define yylex		qp_scanner_lex
#define yywrap		qp_scanner_wrap

#endif

#endif	/*	CERN_QP_LEXYACC	*/
