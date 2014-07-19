/*
 *  qp_report.h  -  error reporting in the sf package
 *
 *  Original: 17-Aug-1994 16:13
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_report.h,v 1.5 1996/05/06 09:18:13 maartenb Exp $
 *
 *  $Log: qp_report.h,v $
 *  Revision 1.5  1996/05/06 09:18:13  maartenb
 *  - Introduce a consistent reporting of array bound errors
 *
 *  - Cleanup dynamic non scalar access.
 *
 *  Revision 1.4  1996/04/23 18:38:55  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef _qp_report_h
#define _qp_report_h

#include	<setjmp.h>


void sf_report( char * const, ... );

void qp_report_bounds( long evt, char * name, int idx, int start, int end, int max);

extern int     qp_abort_env_valid;
extern jmp_buf qp_abort_env;

void qp_abort( char * const, ... );


#ifdef NDEBUG
#define qp_assert(_EX)  ((void)0)
#else
#define qp_assert(_EX) ((_EX) ? (void)0 : qp_abort("Assertion failed: " #_EX \
		", file %s, line %d\n", __FILE__, __LINE__))
#endif

#endif	/*	_qp_report_h	*/
