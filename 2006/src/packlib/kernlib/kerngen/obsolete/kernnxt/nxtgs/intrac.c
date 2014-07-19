/*
 * $Id: intrac.c,v 1.1.1.1 1996/02/15 17:53:30 mclareni Exp $
 *
 * $Log: intrac.c,v $
 * Revision 1.1.1.1  1996/02/15 17:53:30  mclareni
 * Kernlib
 *
 */
#include "kernnxt/pilot.h"
/*>    ROUTINE INTRAC
  CERN PROGLIB# Z044    INTRAC          .VERSION KERNNXT  1.00  901105
  */
#if defined(CERNLIB_QX_SC)
long intrac_()
#endif
#if defined(CERNLIB_QXNO_SC)
long intrac()
#endif
#if defined(CERNLIB_QXCAPT)
long INTRAC()
#endif
{
    return ((long) isatty(0));
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_TCGEN_INTRAC
#undef CERNLIB_TCGEN_INTRAC
#endif
