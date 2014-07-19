*
* $Id: cduvtx.F,v 1.1.1.1 1996/02/28 16:24:32 mclareni Exp $
*
* $Log: cduvtx.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUVTX (IDAY, ITIM)
*     ==============================
*
************************************************************************
*                                                                      *
*        SUBR. CDUVTX (IDAY*, ITIM*)                                   *
*                                                                      *
*   Returns the current date and time                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDAY     Date in YYMMDD                                          *
*     ITIM     Time in seconds                                         *
*                                                                      *
*   Called by CDFOPN, DBxxxx in the Example patches                    *
*                                                                      *
************************************************************************
*
#if !defined(CERNLIB_VAX)
      COMMON /SLATE/  ISLAT(40)
#endif
      INTEGER         IDAY, ITIM
*
*     ------------------------------------------------------------------
*
* ** Get date and time for constructing the return arguments
*
#if !defined(CERNLIB_VAX)
      CALL DATIME (IDAY, ITIM)
      IDAY = MOD (IDAY, 1000000)
      ITIM = ISLAT(4) * 3600 + ISLAT(5) * 60 + ISLAT(6)
#endif
#if defined(CERNLIB_VAX)
      CALL LIB$DAY (IDAY,,ITIM)
#endif
*                                                             END CDUVTX
      END
