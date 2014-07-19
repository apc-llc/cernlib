/*
 * $Id: pavercmd.c,v 1.7 1998/11/10 09:38:03 couet Exp $
 *
 * $Log: pavercmd.c,v $
 * Revision 1.7  1998/11/10 09:38:03  couet
 * - Y2K
 *
 * Revision 1.6  1998/04/09 13:44:18  couet
 * - New output format
 *
 * Revision 1.5  1997/01/08 16:14:37  couet
 * - printing of the COMIS version added in the command version
 *
 * Revision 1.4  1996/10/17 14:37:23  couet
 * - prototype for ku_vers was missing
 *
 * Revision 1.3  1996/10/17 11:46:45  couet
 * - include stdio was missing
 *
 * Revision 1.2  1996/10/14 09:11:42  cremel
 * Add call to ku_vers to print the KUIP version in PAW command version.
 *
 * Revision 1.1.1.1  1996/03/01 11:38:59  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.07/19 21/11/95  18.48.49  by  Unknown*/
/*-- Author :    Maarten Ballintijn   01/03/95*/

/*
 *  Implementation of the version command
 *
 */

#include <stdio.h>

/* arange external symbol ... */
#if defined(CERNLIB_QX_SC)
#define         pavercmd        pavercmd_
#endif
#if defined(CERNLIB_IBM)
#pragma         linkage(pavercmd,FORTRAN)
#endif
#if defined(CERNLIB_CRAY)
#define         pavercmd        PAVERCMD
#endif

char    *pavers();
char    *ku_vers();
char    *hbvers();
char    *hplvers();
char    *igvers();
char    *sivers();
char    *csvers();
#if 0
char    *mnvers();
#endif


void
#if !defined(CERNLIB_WINNT)
pavercmd()
#endif
#if defined(CERNLIB_WINNT)
__stdcall PAVERCMD()
#endif
{
printf("\n");
printf("            +---------+--------+---------------------+\n");
printf("            | Package | Vers.# |  Last modified at   |\n");
printf("            +---------+--------+---------------------+\n");
printf("            | %s |\n", pavers() );
printf("            | %s |\n", ku_vers() );
printf("            | %s |\n", hbvers() );
printf("            | %s |\n", csvers() );
printf("            | %s |\n", hplvers() );
printf("            | %s |\n", igvers() );
printf("            | %s |\n", sivers() );
printf("            +----------------------------------------+\n");
}
