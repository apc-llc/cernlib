/*
 * $Id: ishftr.c,v 1.1 2006/09/15 09:35:25 mclareni Exp $
 *
 * $Log: ishftr.c,v $
 * Revision 1.1  2006/09/15 09:35:25  mclareni
 * Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
 *
 * Revision 1.1.1.1  1996/02/15 17:50:07  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
/*>    ROUTINE ISHFT
  CERN PROGLIB#         ISHFTR          .VERSION KERNLNX  1.02  940511

  Logical right shift by *len (+ve) places
*/
unsigned int ishftr_(arg,len)
unsigned int *arg;
int *len;
{
   return(*arg >> *len);
}
/*> END <----------------------------------------------------------*/
