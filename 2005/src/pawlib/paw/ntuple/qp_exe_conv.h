/*
 *  qp_exe_conv.h  --
 *
 *  Original: 25-Jan-1995 16:10
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_conv.h,v 1.2 1996/04/23 18:38:24 maartenb Exp $
 *
 *  $Log: qp_exe_conv.h,v $
 *  Revision 1.2  1996/04/23 18:38:24  maartenb
 *  - Add RCS keywords
 *
 *
 */

{
	Int32		*r, *o1;

	switch ( fc ) {

#include	"qp_exe_conv_case.h"


	default:
		sf_report( "qp_exe_conv.h: Unkown Fcode ( %d )\n", fc );
		*errp = R_INTERNAL_ERROR;
		running = FALSE;
		break;
	}
}
