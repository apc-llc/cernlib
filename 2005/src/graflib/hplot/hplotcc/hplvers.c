/*
 * $Id: hplvers.c,v 1.19 2002/11/12 15:34:33 couet Exp $
 *
 * $Log: hplvers.c,v $
 * Revision 1.19  2002/11/12 15:34:33  couet
 * -5.24/02
 *
 * Revision 1.18  2002/05/21 13:00:14  couet
 * - mistake in the date
 *
 * Revision 1.17  2002/04/04 10:25:19  couet
 * - 5.24/01
 *
 * Revision 1.16  2001/12/07 15:49:50  couet
 * - 5.24/00
 *
 * Revision 1.15  2001/03/06 15:11:26  couet
 * - 5.23/04
 *
 * Revision 1.14  2000/09/06 09:46:13  couet
 * - 5.23/03
 *
 * Revision 1.13  2000/06/06 13:46:09  couet
 * - 5.23/02
 *
 * Revision 1.12  2000/04/10 13:54:09  couet
 * - 5.23/01
 *
 * Revision 1.11  2000/03/16 15:37:37  couet
 * - 5.23/00
 *
 * Revision 1.10  1999/05/18 09:00:40  couet
 * - 5.22/00
 *
 * Revision 1.9  1998/12/07 13:47:51  couet
 * - 5.21/04
 *
 * Revision 1.8  1998/12/02 15:08:55  couet
 * *** empty log message ***
 *
 * Revision 1.7  1998/12/02 15:08:14  couet
 * - 5.21/03
 *
 * Revision 1.6  1998/11/10 09:25:46  couet
 * - Y2K
 *
 * Revision 1.5  1998/10/26 16:48:34  couet
 * - 5.21/02
 *
 * Revision 1.4  1998/07/08 13:40:27  couet
 * - 5.21/01
 *
 * Revision 1.3  1998/06/02 09:21:17  couet
 * - vers ++
 *
 * Revision 1.2  1998/04/09 13:05:00  couet
 * - Compilation date removed from the vidqq string: it was meaningless
 *
 * Revision 1.1.1.1  1996/01/19 10:50:15  mclareni
 * Hplot
 *
 *-- Author :    O.Couet   01/03/95
 */

static char vidqq[] = "@(#)HPLOT    5.24/02  12/11/2002  16.30.00";

/*
 *  Return the static version string for HPLOT
 */

char * hplvers()
{
        /* skip the 4 char that contain the "what" string */
        return &vidqq[4];
}
