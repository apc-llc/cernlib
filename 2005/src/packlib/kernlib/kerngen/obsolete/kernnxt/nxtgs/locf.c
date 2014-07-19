/*
 * $Id: locf.c,v 1.1.1.1 1996/02/15 17:53:31 mclareni Exp $
 *
 * $Log: locf.c,v $
 * Revision 1.1.1.1  1996/02/15 17:53:31  mclareni
 * Kernlib
 *
 */
#include "kernnxt/pilot.h"
/*>    ROUTINE LOCF
  CERN PROGLIB# N100    LOCF            .VERSION KERNNXT  1.00  901105
  */
#if defined(CERNLIB_QX_SC)
long locf_(iadr)
#endif
#if defined(CERNLIB_QXNO_SC)
long locf(iadr)
#endif
#if defined(CERNLIB_QXCAPT)
long LOCF(iadr)
#endif
#define NADUPW 4   /* Number of ADdress Units Per Word */
#define LADUPW 2   /* Logarithm base 2 of ADdress Units Per Word */
char *iadr;
{
   return( (unsigned) iadr / NADUPW );
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_TCGEN_LOCF
#undef CERNLIB_TCGEN_LOCF
#endif
