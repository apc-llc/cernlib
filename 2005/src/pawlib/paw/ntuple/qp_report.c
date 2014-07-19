/*
 *  qp_report.c  --
 *	implement sf package related error reporting.
 *
 *  Original: 11-Oct-1994 13:57
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_report.c,v 1.8 1996/05/06 09:18:12 maartenb Exp $
 *
 *  $Log: qp_report.c,v $
 *  Revision 1.8  1996/05/06 09:18:12  maartenb
 *  - Introduce a consistent reporting of array bound errors
 *
 *  - Cleanup dynamic non scalar access.
 *
 *  Revision 1.7  1996/04/23 18:38:54  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<setjmp.h>
#include	<stdarg.h>
#include	<stdio.h>
#include	<stdlib.h>

#include	"qp_report.h"
#include	"kuip_interface.h"


void
sf_report( char *const fmt, ... )
{
	va_list	ap;

	printf( "%s: ", ku_path() );

	va_start( ap, fmt );
	vprintf( fmt, ap );
	va_end( ap );

	fflush(stdout);
}


void
qp_report_bounds( long evt, char * name, int index, int start, int end, int max)
{
	char	header[128];

	if ( evt != -1 ) {
		sf_report( "ERROR: at event %ld\n", evt );
	}
	sprintf( header, "Array bounds error: index %d of '%s'", index, name );
	if ( start == end ) {
		sf_report( "%s [1..%d]\n\trequested value = %d\n", header,
			max, start );
	} else {
		sf_report( "%s [1..%d]\n\trequested range = %d : %d\n", header,
			max, start, end );
	}
}


int	qp_abort_env_valid;
jmp_buf qp_abort_env;

void
qp_abort( char *const fmt, ... )
{
	va_list	ap;

	printf( "%s: ", ku_path() );

	va_start( ap, fmt );
	vprintf( fmt, ap );
	va_end( ap );

	fflush(stdout);

	if ( qp_abort_env_valid != 0 ) {
		longjmp( qp_abort_env, 1 );
	} else {
		abort();
	}
}
