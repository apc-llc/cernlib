/*
 * $Id: kuvers.c,v 1.19 2003/02/13 14:27:02 couet Exp $
 *
 * $Log: kuvers.c,v $
 * Revision 1.19  2003/02/13 14:27:02  couet
 * - 2.09/00
 *
 * Revision 1.18  2001/05/23 15:22:36  couet
 * - 2.08/03
 *
 * Revision 1.17  2000/01/24 09:25:48  couet
 * - 2.08/02
 *
 * Revision 1.16  2000/01/21 13:31:01  couet
 * - 2.08/01
 *
 * Revision 1.15  2000/01/10 10:49:50  couet
 * - 2.08/00
 *
 * Revision 1.14  1999/11/24 10:07:24  couet
 * - 2.07/18
 *
 * Revision 1.13  1999/01/05 14:45:51  couet
 * - 2.07/17
 *
 * Revision 1.12  1998/11/10 09:50:31  couet
 * - Y2K
 *
 * Revision 1.11  1998/04/09 12:57:00  couet
 * - Compilation date removed from the vidqq string: it was meaningless
 *
 * Revision 1.10  1997/09/25 14:02:27  cremel
 * VECTOR WRITE (kxwriv.F): increase limitation of number of vectors in the
 * list VLIST from 10 to 30 (same value as in kxreav.F for VECTOR/READ).
 * Update HELP for VECTOR/WRITE and VECTOR/READ and mention explicitely this
 * limitation. Improve error message in kxwriv.F and kxreav.F by
 * adding explicitely VECTOR/WRITE and VECTOR/READ.
 * + Increase KUIP version number to 2.07/16  (25/09/97).
 *
 * Revision 1.9  1997/08/29 10:08:55  cremel
 * Increase version number to 2.07/15.
 *
 * Revision 1.8  1997/07/08 09:48:22  cremel
 * New version 2.07/14 :
 * bug correction in VECTOR/READ (for free FORMAT suppress the limitation of
 * 80 characters and protect the code when number of tokens > 50) -
 * see example: vec2.kumac + vec2.dat + vec2bad.dat
 *
 * Revision 1.7  1997/06/18 12:13:06  couet
 * - vers ++
 *
 * Revision 1.6  1997/03/11 09:43:44  cremel
 * Bug correction in command WAIT (after answering "Q" to a "WAIT" statement,
 * the ability to issue several HISTOGRAM/PLOT statements on a single line,
 * separated by semicolons, disappears).
 * kexec.c : add a test on the variable "kc_flags.in_macro".
 * kuvers.c : increase version number to 2.07/13
 *
 * Revision 1.5  1997/03/04 14:03:15  cremel
 * Increase KUIP version to 2.07/12 with the date of modification (commit)
 * for routine kxlisv.F (bug correction in VECTOR/LIST).
 *
 * Revision 1.4  1997/01/21 08:15:55  cremel
 * Increase version number for KUIP: 2.07/11
 *
 * Revision 1.3  1996/10/24 12:08:08  cremel
 * Version 2.07/10
 *
 * Revision 1.2  1996/10/14 09:01:34  cremel
 * Change place of initialization for do_exit and do_quit (from kuwhat to
 * kuinit) in order to allow "exit" in pawlogon.kumac.
 *
 * Revision 1.1.1.1  1996/03/08 15:32:58  mclareni
 * Kuip
 *
 */
/*-- Author :    Maarten Ballintijn   01/03/95*/

static char vidqq[] = 
"@(#)KUIP     2.09/00  13/02/2003  15:25.00";

/*
 *  Return the static version string for KUIP
 */

char * ku_vers()
{
        /* skip the 4 char that contain the "what" string */
        return &vidqq[4];
}
