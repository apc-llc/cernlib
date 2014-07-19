/*
 *  hbook_defs.h  --
 *	Maximum sizes and numbers hard-coded into HBOOK.
 *
 *  Original: 18-Jan-1995 14:22
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: hbook_defs.h,v 1.4 1996/04/23 18:38:02 maartenb Exp $
 *
 *  $Log: hbook_defs.h,v $
 *  Revision 1.4  1996/04/23 18:38:02  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_HBOOK_DEFS
#define CERN_HBOOK_DEFS

/* The maximum length of a RZ pathname argument */

#define	MAX_RZ_PATH	1024		/* is this defined somewhere ?? */

/* The maximum length of ntuple variable (column) name */

#define	MAX_NAME_LEN	32


/* The maximum length of ntuple block name */

#define	MAX_BLOCK_LEN	8


/* The maximum number of dimensions in a variable	*/
/* this is actualy only 7, but the define doubles as	*/
/* the maximum number of arguments to a routine		*/
/* maybe it should be even raised ?			*/

#define	MAX_DIMENSION	10


/* The maximum number of columns in a row wise ntuple */

#define		MAX_RWN_COLS	512


/* The maximum number of characters in a histogram label (for hlabel etc.) */

#define	MAX_LABEL_LEN	16


#endif	/*	CERN_HBOOK_DEFS	*/
