/*
 * $Id: pavers.c,v 1.99 2004/01/14 08:25:11 couet Exp $
 *
 * $Log: pavers.c,v $
 * Revision 1.99  2004/01/14 08:25:11  couet
 * - 2.14/04
 *
 * Revision 1.98  2003/06/04 15:09:31  couet
 * - 2.14/03
 *
 * Revision 1.97  2003/02/07 12:21:58  couet
 * *** empty log message ***
 *
 * Revision 1.96  2003/01/31 09:39:32  couet
 * *** empty log message ***
 *
 * Revision 1.95  2002/10/17 07:02:26  couet
 * *** empty log message ***
 *
 * Revision 1.94  2002/09/20 07:55:37  couet
 * *** empty log message ***
 *
 * Revision 1.93  2002/09/04 09:43:12  couet
 * *** empty log message ***
 *
 * Revision 1.92  2002/06/19 15:00:46  couet
 * - 2.13/06
 *
 * Revision 1.91  2002/01/15 16:12:08  couet
 * - 2.13/05
 *
 * Revision 1.90  2001/12/07 15:54:21  couet
 * - 2.13/04
 *
 * Revision 1.89  2001/10/02 15:06:55  couet
 * - 2.13/03
 *
 * Revision 1.88  2001/09/25 13:12:59  couet
 * - 2.13/02
 *
 * Revision 1.87  2001/09/18 14:15:45  couet
 * - 2.13/01
 *
 * Revision 1.86  2001/07/17 14:50:53  couet
 * - 2.13/00
 *
 * Revision 1.85  2001/06/13 14:18:05  couet
 * - 2.12/22
 *
 * Revision 1.84  2001/05/21 09:14:35  couet
 * - 2.12/21
 *
 * Revision 1.83  2001/04/12 12:54:37  couet
 * - 2.12/20
 *
 * Revision 1.82  2001/04/03 09:29:27  couet
 * - 2.12/19
 *
 * Revision 1.81  2001/03/14 16:18:44  couet
 * - 2.12/18
 *
 * Revision 1.80  2001/02/20 15:17:28  couet
 * - 2.12/17
 *
 * Revision 1.79  2001/01/31 13:53:14  couet
 * - 2.12/16
 *
 * Revision 1.78  2000/12/19 17:03:36  couet
 * - 2.12/15
 *
 * Revision 1.77  2000/10/23 16:29:12  couet
 * - 2.12/14
 *
 * Revision 1.76  2000/10/02 16:35:01  couet
 * - 2.12/13
 *
 * Revision 1.75  2000/09/29 15:58:53  couet
 * - 2.12/12
 *
 * Revision 1.74  2000/07/25 11:48:31  couet
 * - 2.12/11
 *
 * Revision 1.73  2000/06/15 13:59:15  couet
 * - 2.12/10
 *
 * Revision 1.72  2000/06/06 14:14:25  couet
 * - 2.12/09
 *
 * Revision 1.71  2000/05/23 09:32:57  couet
 * - 2.12/08
 *
 * Revision 1.70  2000/03/03 14:40:23  couet
 * - 2.12/07
 *
 * Revision 1.69  2000/02/25 12:16:40  couet
 * - 2.12/06
 *
 * Revision 1.68  2000/02/24 17:05:31  couet
 * - 2.12/05
 *
 * Revision 1.67  2000/02/23 10:31:10  couet
 * - 2.12/04
 *
 * Revision 1.66  2000/01/28 16:06:36  couet
 * - 2.12/03
 *
 * Revision 1.65  2000/01/18 10:01:36  couet
 * - 2.12/02
 *
 * Revision 1.64  2000/01/13 15:54:44  couet
 * - 2.12/01
 *
 * Revision 1.63  2000/01/13 10:23:21  couet
 * - 2.12/00
 *
 * Revision 1.62  1999/12/06 16:07:30  couet
 * - 2.11/13
 *
 * Revision 1.61  1999/11/23 17:34:13  couet
 * - 2.11/12
 *
 * Revision 1.60  1999/11/09 10:14:58  couet
 * - 2.11/11
 *
 * Revision 1.59  1999/11/03 10:13:20  couet
 * - 2.11/09
 *
 * Revision 1.58  1999/10/29 15:22:17  couet
 * - 2.11/09
 *
 * Revision 1.57  1999/10/06 14:45:48  couet
 * - 2.11/08
 *
 * Revision 1.56  1999/10/05 07:06:35  couet
 * - 2.11/07
 *
 * Revision 1.55  1999/09/29 08:23:11  couet
 * - 2.11/06
 *
 * Revision 1.54  1999/09/17 07:35:09  couet
 * - 2.11/05
 *
 * Revision 1.53  1999/08/31 08:51:30  couet
 * - 2.11/04
 *
 * Revision 1.52  1999/05/20 07:43:52  couet
 * - 2.11/03
 *
 * Revision 1.51  1999/04/16 14:25:56  couet
 * - 2.11/02
 *
 * Revision 1.50  1999/04/08 15:15:14  couet
 * - 2.11/01
 *
 * Revision 1.49  1999/03/25 14:24:04  couet
 * - 2.11/00
 *
 * Revision 1.48  1999/03/01 09:16:01  couet
 * - 2.10/09
 *
 * Revision 1.47  1999/01/27 13:50:12  couet
 * - 2.10/08
 *
 * Revision 1.46  1999/01/07 13:07:02  couet
 * - 2.10/07
 *
 * Revision 1.45  1998/12/10 10:55:54  couet
 * - 2.10/06
 *
 * Revision 1.44  1998/12/08 09:14:30  couet
 * - 2.10/05
 *
 * Revision 1.43  1998/11/30 09:36:46  couet
 * - 2.10/04
 *
 * Revision 1.42  1998/11/16 13:53:01  couet
 * - 2.10/03
 *
 * Revision 1.41  1998/11/10 09:33:00  couet
 * - Y2K
 *
 * Revision 1.40  1998/09/17 10:15:42  couet
 * - 2.10/02
 *
 * Revision 1.39  1998/09/02 10:12:53  couet
 * - 2.10/01
 *
 * Revision 1.38  1998/04/09 12:53:57  couet
 * - 2.10/00
 *
 * Revision 1.37  1998/03/19 15:30:59  couet
 * - 2.09/18
 *
 * Revision 1.36  1998/02/12 08:32:01  couet
 * - 2.09/17
 *
 * Revision 1.35  1997/11/24 10:50:55  couet
 * - 2.09/16
 *
 * Revision 1.34  1997/11/18 13:51:58  couet
 * - 2.09/15
 *
 * Revision 1.33  1997/07/18 09:38:24  couet
 * - 2.09/14
 *
 * Revision 1.32  1997/07/10 13:29:53  couet
 * - 2.09/13
 *
 * Revision 1.31  1997/06/20 14:48:39  couet
 * - 2.09/12
 *
 * Revision 1.30  1997/06/13 14:06:16  couet
 * - 2.09/11
 *
 * Revision 1.29  1997/05/20 09:44:20  couet
 * - 2.09/10
 *
 * Revision 1.28  1997/05/13 12:00:59  couet
 * - 2.8/09
 *
 * Revision 1.27  1997/04/25 12:09:21  couet
 * - 2.09/08
 *
 * Revision 1.26  1997/04/23 15:03:06  couet
 * - 2.09/07
 *
 * Revision 1.25  1997/04/08 12:34:13  couet
 * - 2.09/06
 *
 * Revision 1.24  1997/04/03 12:47:48  couet
 * - version ++
 *
 * Revision 1.23  1997/03/17 10:02:56  couet
 * - 2.09/04
 *
 * Revision 1.22  1997/03/14 11:16:52  couet
 * - 2.09/03
 *
 * Revision 1.21  1997/03/11 16:17:59  couet
 * - Version 2.09/02
 *
 * Revision 1.20  1997/03/10 17:02:30  couet
 * - vers ++
 *
 * Revision 1.19  1997/02/12 14:22:25  couet
 * Version 2.08/15      12 February 1997
 *
 * Revision 1.18  1997/01/21 16:49:48  couet
 * - version ++
 *
 * Revision 1.17  1997/01/09 12:47:33  couet
 * - version 2.08/13
 *
 * Revision 1.16  1996/12/05 10:10:31  lecointe
 * change version to 2.08/12
 *
 * Revision 1.15  1996/10/21 16:40:22  couet
 * - 2.08/11
 *
 * Revision 1.14  1996/10/15 16:10:17  couet
 * - Version 2.08/10
 *
 * Revision 1.13  1996/10/03 11:40:50  couet
 * - version++
 *
 * Revision 1.12  1996/09/19 09:26:46  couet
 * - version++
 *
 * Revision 1.11  1996/09/12 09:43:40  couet
 * - V 2.08/07
 *
 * Revision 1.10  1996/08/27 11:45:12  couet
 * - version 2.08/06
 *
 * Revision 1.9  1996/07/11 09:07:20  couet
 * - new version
 *
 * Revision 1.8  1996/07/04 13:39:56  couet
 * - version 2.08/04
 *
 * Revision 1.7  1996/06/24 08:48:10  couet
 * - New version
 *
 * Revision 1.6  1996/06/17 09:16:20  couet
 * - Version number incremented
 *
 * Revision 1.5  1996/06/13 15:10:32  couet
 * - Version number incremented
 *
 * Revision 1.4  1996/06/03 14:03:35  couet
 * - paw version is now 2.08/00
 *
 * Revision 1.3  1996/05/13 16:43:20  couet
 * - Version number incremented
 *
 * Revision 1.2  1996/03/14 17:37:48  couet
 * Change the version number
 *
 * Revision 1.1.1.1  1996/03/01 11:38:59  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"

static char vidqq[] = 
"@(#)PAW      2.14/04  12/01/2004  10.45.00";

/*
 *  Return the static version string for PAW
 */

char *
pavers()
{
        /* skip the 4 char that contain the "what" string */
        return &vidqq[4];
}
