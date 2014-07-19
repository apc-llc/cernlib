/*
 * $Id: hbvers.c,v 1.27 2003/06/04 15:03:55 couet Exp $
 *
 * $Log: hbvers.c,v $
 * Revision 1.27  2003/06/04 15:03:55  couet
 * - 4.28/01
 *
 * Revision 1.26  2003/02/07 12:15:34  couet
 * - 4.28/00
 *
 * Revision 1.25  2001/12/12 16:23:34  couet
 * - 4.27/03
 *
 * Revision 1.24  2001/10/10 08:24:41  couet
 * - 4.27/02
 *
 * Revision 1.23  2001/10/02 09:08:37  couet
 * - 4.27/01
 *
 * Revision 1.22  2001/09/24 14:30:19  couet
 * - 4.27/00
 *
 * Revision 1.21  2000/12/19 16:59:24  couet
 * - 4.26/01
 *
 * Revision 1.20  2000/01/25 17:10:02  couet
 * - 4.26/00
 *
 * Revision 1.19  1999/03/23 14:37:30  couet
 * vers ++
 *
 * Revision 1.18  1999/03/05 15:44:14  couet
 * - ver++
 *
 * Revision 1.17  1999/02/18 09:53:38  couet
 * - 4.25/02
 *
 * Revision 1.16  1998/11/10 09:24:46  couet
 * - 4.25/01 and Y2K
 *
 * Revision 1.15  1998/06/11 13:29:38  couet
 * - 4.25/00
 *
 * Revision 1.14  1998/04/09 13:00:01  couet
 * - Compilation date removed from the vidqq string: it was meaningless
 *
 * Revision 1.13  1998/03/20 09:46:43  couet
 * - 4.24/11
 *
 * Revision 1.12  1998/01/06 13:01:12  couet
 * - version ++
 *
 * Revision 1.11  1997/09/18 07:26:12  couet
 * - 4.24/09
 *
 * Revision 1.10  1997/08/20 16:10:08  couet
 * error in the date
 *
 * Revision 1.9  1997/08/20 16:04:02  couet
 * - vers ++
 *
 * Revision 1.8  1997/05/13 15:27:10  couet
 * - version++
 *
 * Revision 1.7  1997/02/27 14:46:31  couet
 * - 4.24/06
 *
 * Revision 1.6  1997/02/25 14:14:33  couet
 * - 4.24/05
 *
 * Revision 1.5  1997/02/21 12:23:21  couet
 * - 4.24/04
 *
 * Revision 1.4  1997/01/17 09:11:48  couet
 * - version ++
 *
 * Revision 1.3  1997/01/10 17:50:28  couet
 * version++
 *
 * Revision 1.2  1996/11/18 13:52:38  couet
 * - Version ++
 *
 * Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
 * First import
 *
 */
#include "hbook/pilot.h"

static char vidqq[] = 
"@(#)HBOOK    4.28/01  04/06/2003  17.00.00";
 
/*
 *  Return the static version string for HBOOK
 */
 
char *
hbvers()
{
	/* skip the 4 char that contain the "what" string */
	return &vidqq[4];
}
