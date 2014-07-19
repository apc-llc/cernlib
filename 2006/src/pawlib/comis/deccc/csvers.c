/*
 * $Id: csvers.c,v 1.21 2004/10/27 09:03:27 couet Exp $
 *
 * $Log: csvers.c,v $
 * Revision 1.21  2004/10/27 09:03:27  couet
 * - 1.24/00
 *
 * Revision 1.20  2002/06/19 14:29:46  couet
 * - 1.23/01
 *
 * Revision 1.19  2002/06/14 08:11:55  couet
 * - version number was wrong
 *
 * Revision 1.18  2001/09/14 13:08:52  mclareni
 * Undo the previous change, as the required option -check nopower has been added to osf1.cf
 *
 * Revision 1.17  2001/09/04 13:18:41  couet
 * - 1.23/00
 *
 * Revision 1.16  2001/02/07 09:37:20  couet
 * - 1.22/03
 *
 * Revision 1.15  2000/09/25 14:18:36  couet
 * - 1.22/02
 *
 * Revision 1.14  2000/05/30 13:56:34  couet
 * - 1.22/01
 *
 * Revision 1.13  2000/01/27 13:43:06  couet
 * - 1.22/00
 *
 * Revision 1.12  1998/11/10 10:02:07  couet
 * - Y2K
 *
 * Revision 1.11  1998/05/13 08:19:31  couet
 * - 1.21/00
 *
 * Revision 1.10  1998/04/09 13:02:35  couet
 * - Compilation date removed from the vidqq string: it was meaningless
 *
 * Revision 1.9  1998/03/11 12:49:14  couet
 * - 1.20/05
 *
 * Revision 1.8  1998/02/27 09:28:29  couet
 * - 1.20/04
 *
 * Revision 1.7  1998/01/16 11:28:45  couet
 * - 1.20/03
 *
 * Revision 1.6  1997/06/03 10:01:39  couet
 * - 1.20/02
 *
 * Revision 1.5  1997/05/13 11:58:58  couet
 * - version++
 *
 * Revision 1.4  1997/04/23 15:06:33  couet
 * - 1.20/00
 *
 * Revision 1.3  1997/02/20 12:35:21  couet
 * - version ++
 *
 * Revision 1.2  1996/10/31 20:15:43  couet
 * - 1.19/02
 *
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"

static char vidqq[] = 
"@(#)COMIS    1.24/00  27/10/2004  11:00.00";

/*
 *  Return the static version string for COMIS
 */

char *
csvers()
{
	/* skip the 4 char that contain the "what" string */
	return &vidqq[4];
}
