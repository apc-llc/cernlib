*
* $Id: cdtopn.F,v 1.1.1.1 1996/02/28 16:24:32 mclareni Exp $
*
* $Log: cdtopn.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDTOPN (PATHN, TOPN, NCH)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDTOPN (PATHN, TOPN*, NCH*)                             *
*                                                                      *
*   Extracts the top directory name from the path name                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     TOPN     Name of the top directory                               *
*     NCH      Number of characters in TOPN                            *
*                                                                      *
*   Called by CDFZUP, CDLDUP, CDMDIR                                   *
*                                                                      *
************************************************************************
*
      CHARACTER       PATHN*(*), TOPN*(*)
*
*     ------------------------------------------------------------------
*
      NCTOT  = LENOCC (PATHN)
      I1     = 0
      I11    = 0
      DO 10 I0 = 1, NCTOT
        IF (PATHN(I0:I0).EQ.'/') THEN
          IF (I1.GT.0) THEN
            TOPN   = PATHN(I11:I0-1)
            NCH    = I0 - I11
            GO TO 15
          ENDIF
        ELSE
          IF (I1.EQ.0) I11 = I0
          I1     = I1 +1
        ENDIF
   10 CONTINUE
      IF (I1.GT.0) THEN
        TOPN   = PATHN(I11:NCTOT)
        NCH    = NCTOT - I11 + 1
      ELSE
        NCH    = 0
        TOPN   = ' '
      ENDIF
   15 CONTINUE
*                                                             END CDTOPN
      END
