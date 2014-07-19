/*
 * $Id: sigon.c,v 1.1.1.1 1996/02/15 17:52:43 mclareni Exp $
 *
 * $Log: sigon.c,v $
 * Revision 1.1.1.1  1996/02/15 17:52:43  mclareni
 * Kernlib
 *
 */
#include "kerncry/pilot.h"
/*>    ROUTINE SIGON
  CERN PROGLIB#         SIGON           .VERSION KERNIRT  1.03  910314
  ORIG. 25/10/91, JZ
  Fortran interface routine to sigon / sigoff
*/
int  SIGON()
{
      return ((int) sigon());
}
int  SIGOFF()
{
      return ((int) sigoff());
}
/*> END <----------------------------------------------------------*/
