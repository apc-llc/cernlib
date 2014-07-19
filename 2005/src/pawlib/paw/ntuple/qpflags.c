/*
 *  qpflags.c  --
 *	Manage a set of debugging flags for the QP package
 *
 *  Original: 25-Jan-1995 11:05
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qpflags.c,v 1.10 1999/06/28 15:09:06 couet Exp $
 *
 *  $Log: qpflags.c,v $
 *  Revision 1.10  1999/06/28 15:09:06  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.9  1996/04/23 18:39:08  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<stdio.h>

#ifdef	HAS_SYS_TIMES
#include	<sys/times.h>
#include	<unistd.h>
#endif

#include	<cfortran/cfortran.h>
#include	"kuip_interface.h"


typedef struct _option_list_ {
	char	*option;
	int	value;
	char	*usage;
} OptionList;

static OptionList	options[] = {
	{ "verbose",	0, "more diagnostic output" },
	{ "symtab",	0, "log calls to the symtab routines" },
	{ "name",	0, "log calls to the name resolving routines" },
	{ "stack",	0, "log calls to the stack handling macros" },
	{ "cache",	0, "log use of the in memory ntuple cache" },
	{ "dump",	0, "insert code for dump of final stack frames" },
	{ "trace",	0, "print event number and resulting stack frames" },
	{ "tree",	0, "print the semantically analyzed syntax tree" },
	{ "seg",	0, "print the bytecode segments" },
	{ "stat_eval",	1, "compile time eval of expressions" },
	{ 0,		0 },
};


static OptionList *
find_option( char * option )
{
	OptionList	*op;

	op = options;
	for ( ; op->option != 0 ; op += 1 ) {
		if ( strcasecmp( op->option, option ) == 0 ) {
			return op;
		}
	}

	return 0;
}


static void
qp_flags_print_all()
{
	OptionList	*op;

	op = options;
	for ( ; op->option != 0 ; op += 1 ) {
		printf( "    %12s = %d (%s)\n", op->option, op->value, op->usage );
	}

	putchar( '\n' );
}


static void
qp_flags_print( char *option )
{
	OptionList	*op;

	op = find_option( option );
	if ( op != 0 ) {
		printf( "    %s = %d (%s)\n\n", op->option, op->value, op->usage );
	} else {
		printf( "    %s does not exist !\n\n", option );
	}
}


int
qp_flags_set( char *option, int value )
{
	OptionList	*op;
	int		err;

	op = find_option( option );
	if ( op != 0 ) {
		op->value = value;
		printf( "    %s = %d\n\n", op->option, op->value );
		err = 0;
	} else {
		printf( "    %s does not exist !\n\n", option );
		err = 1;
	}

	return err;
}


int
qp_flags_get( char *option )
{
	OptionList	*op;
	int		value;

	op = find_option( option );
	if ( op != 0 ) {
		value = op->value;
	} else {
		printf( "    %s does not exist !\n\n", option );
		value = 0;
	}

	return value;
}


void
qpflags_C( void )
{
	int	npar;
	char	option[128];
	int	value;

	npar = ku_npar();

	switch ( npar ) {
	case 0:			/* list all options + settings */
		qp_flags_print_all();
		break;
	case 1:			/* list this options + settings */
		strncpy( option, ku_getf(), sizeof( option ) );
		qp_flags_print( option );
		break;
	case 2:			/* set this option */
		strncpy( option, ku_getf(), sizeof( option ) );
		value = ku_geti();
		qp_flags_set( option, value );
		break;
	default:
		printf( "Usage: qpflags [ option [ value ]]\n" );
		break;
	}
}


FCALLSCSUB0(qpflags_C,QPFLAGS,qpflags)	/* create fortran entry point */


/*
 *  Routines to print run times
 */

#ifdef	HAS_SYS_TIMES

static struct tms	t0;
static double		ticks;

void
qptime( int flag )
{
	struct tms	t1;
	double	ut, st;

	if ( flag == 0 ) {
		if ( ticks == 0 ) {
			ticks = sysconf(_SC_CLK_TCK);
		}
		printf( "TIME  <start>\n" );
		fflush( stdout );
		times( &t0 );
	} else {
		times( &t1 );

		ut = (t1.tms_utime - t0.tms_utime) / ticks;
		st = (t1.tms_stime - t0.tms_stime) / ticks;

		printf( "TIME  user %14.3lf system %14.3lf total %14.3lf\n",
			ut, st, ut + st );
		fflush( stdout );
	}
}

#else

void
qptime( int flag )
{
	if ( flag == 0 ) {
		printf( "TIME  <start> (not implemented)\n" );
		fflush( stdout );
	} else {
		printf( "TIME  <end> (not implemented)\n" );
		fflush( stdout );
	}
}

#endif


void
qptime_( int *pflag )
{
	qptime( *pflag );
}
