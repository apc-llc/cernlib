/*
 * $Id: igvers.c,v 1.38 2004/06/10 07:55:22 couet Exp $
 *
 * $Log: igvers.c,v $
 * Revision 1.38  2004/06/10 07:55:22  couet
 * - 1.29/04
 *
 * Revision 1.37  2003/07/23 09:48:18  couet
 * - 1.29/03
 *
 * Revision 1.36  2003/03/18 13:39:13  couet
 * - 1.29/02
 *
 * Revision 1.35  2003/02/17 10:02:29  couet
 * - 1.29/01
 *
 * Revision 1.34  2002/12/04 10:58:39  couet
 * - 1.29/00
 *
 * Revision 1.33  2002/05/22 09:18:47  couet
 * - 1.28/07
 *
 * Revision 1.32  2002/05/16 15:32:11  couet
 * - 1.28/06
 *
 * Revision 1.31  2002/04/04 10:21:28  couet
 * - 1.28/05
 *
 * Revision 1.30  2002/03/18 16:23:41  couet
 * - 1.28/04
 *
 * Revision 1.29  2001/12/07 15:52:27  couet
 * - 1.28/03
 *
 * Revision 1.28  2001/11/23 16:09:25  couet
 * - 1.28/02
 *
 * Revision 1.27  2001/10/17 14:42:54  couet
 * - 1.28/01
 *
 * Revision 1.26  2001/10/08 16:18:06  couet
 * - 1.28/00
 *
 * Revision 1.25  2001/05/31 08:08:24  couet
 * - 1.27/03
 *
 * Revision 1.24  2000/08/29 14:04:06  couet
 * - 1.27/02
 *
 * Revision 1.23  2000/07/14 15:18:59  couet
 * - 1.27/01
 *
 * Revision 1.22  2000/04/03 15:41:39  couet
 * - 1.27/00
 *
 * Revision 1.21  1999/10/20 14:16:52  couet
 * - 1.26/04
 *
 * Revision 1.20  1999/10/13 12:35:28  couet
 * - 1.26/03
 *
 * Revision 1.19  1999/10/06 14:05:19  couet
 * - 1.26/02
 *
 * Revision 1.18  1999/06/21 16:48:37  couet
 * - 1.26/01
 *
 * Revision 1.17  1999/05/06 06:59:21  couet
 * - version 1.26/00 to tag the following bug fixe in x11int.c:
 *  "MapNotify is not supported by all the X11 emulators (problem on Mac !!!)"
 *
 * Revision 1.16  1999/01/07 15:31:27  couet
 * - 1.25/05
 *
 * Revision 1.15  1998/12/07 13:45:23  couet
 * - 1.25/04
 *
 * Revision 1.14  1998/11/16 11:13:28  couet
 * - 1.25/03
 *
 * Revision 1.13  1998/11/13 10:52:29  couet
 * - 1.25/02
 *
 * Revision 1.12  1998/11/10 09:27:12  couet
 * - Y2K
 *
 * Revision 1.11  1998/05/26 09:22:54  couet
 * - 1.25/01
 *
 * Revision 1.10  1998/04/09 13:07:38  couet
 * - Compilation date removed from the vidqq string: it was meaningless
 *
 * Revision 1.9  1998/01/27 14:09:15  couet
 * - 1.24/03
 *
 * Revision 1.8  1997/11/24 15:40:34  couet
 * - 1.24/02
 *
 * Revision 1.7  1997/06/20 15:13:41  couet
 * - 1.24/01
 *
 * Revision 1.6  1997/04/22 08:45:14  couet
 * - 1.24/00
 *
 * Revision 1.5  1997/01/30 10:06:19  couet
 * - version ++
 *
 * Revision 1.4  1996/11/20 15:16:01  couet
 * - version++
 *
 * Revision 1.3  1996/04/23 08:36:50  couet
 *
 * Revision 1.2  1996/02/14 15:18:18  couet
 * The version number was not correct in igvers (CMZ corrupted it).
 *
 * Revision 1.1.1.1  1996/02/14 13:10:25  mclareni
 * Higz
 *
 */
#include "higz/pilot.h"

static char vidqq[] = 
"@(#)HIGZ     1.29/04  10/06/2004  10:00:00";

/*
 *  Return the static version string for HIGZ
 */

char *
igvers()
{
        /* skip the 4 char that contain the "what" string */
        return &vidqq[4];
}
