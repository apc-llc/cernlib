/*
 *  qp_execute.h  --
 *	Declarations for the query interpreter, and
 *	data structures.
 *
 *  Original: 23-Nov-1994 17:06
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_execute.h,v 1.4 1996/04/23 18:38:38 maartenb Exp $
 *
 *  $Log: qp_execute.h,v $
 *  Revision 1.4  1996/04/23 18:38:38  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_EXECUTE
#define CERN_QP_EXECUTE


#include	"qp_query.h"
#include	"qp_command.h"

void
qp_execute(
	QueryExe *	qxp,
	long		start_evt,
	long		nevt,
	QPCmd		*cmd,
	int *		errp
	);

#endif	/*	CERN_QP_EXECUTE	*/
