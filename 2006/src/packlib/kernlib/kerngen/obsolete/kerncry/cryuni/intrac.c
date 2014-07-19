/*
 * $Id: intrac.c,v 1.1.1.1 1996/02/15 17:52:43 mclareni Exp $
 *
 * $Log: intrac.c,v $
 * Revision 1.1.1.1  1996/02/15 17:52:43  mclareni
 * Kernlib
 *
 */
#include "kerncry/pilot.h"
/*>    ROUTINE INTRAC
  CERN PROGLIB# Z044    INTRAC          .VERSION KERNCRY  1.16  911111
*/
#include <fortran.h>
long INTRAC()
{
      int  isatty();
      return (_btol ((long) isatty(0)));
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_CCGEN_INTRAC
#undef CERNLIB_CCGEN_INTRAC
#endif
#ifdef CERNLIB_TCGEN_INTRAC
#undef CERNLIB_TCGEN_INTRAC
#endif
