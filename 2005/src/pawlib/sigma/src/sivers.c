/*
 * $Id: sivers.c,v 1.3 1998/11/10 10:08:32 couet Exp $
 *
 * $Log: sivers.c,v $
 * Revision 1.3  1998/11/10 10:08:32  couet
 * - Y2K
 *
 * Revision 1.2  1998/04/09 13:10:42  couet
 * - Compilation date removed from the vidqq string: it was meaningless
 *
 * Revision 1.1.1.1  1995/12/12 14:36:21  mclareni
 * Imported sources
 *
 */
#include "sigma/pilot.h"

static char vidqq[] = 
"@(#)SIGMA    1.10/03  10/02/1994  17.18.05";

/*
 *  Return the static version string for SIGMA
 */

char *
sivers()
{
	/* skip the 4 char that contain the "what" string */
	return &vidqq[4];
}
