*
* $Id: cdwtop.F,v 1.1.1.1 1996/02/28 16:24:32 mclareni Exp $
*
* $Log: cdwtop.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDWTOP (PATHN, WTOP, NCH)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDWTOP (PATHN, WTOP*, NCH*)                             *
*                                                                      *
*   Extracts the name without the top directory from the path name     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     WTOP     Name without the top directory                          *
*     NCH      Number of characters in WTOP                            *
*                                                                      *
*   Called by CDCDIC, CDLDIC                                           *
*                                                                      *
************************************************************************
*
      CHARACTER       PATHN*(*), WTOP*(*)
*
*     ------------------------------------------------------------------
*
      NCTOT  = LENOCC (PATHN)
      I1     = 0
      DO 10 I0 = 1, NCTOT
        IF (PATHN(I0:I0).EQ.'/') THEN
          IF (I1.GT.0) THEN
            WTOP   = PATHN(I0:NCTOT)
            NCH    = NCTOT - I0 + 1
            GO TO 15
          ENDIF
        ELSE
          I1     = I1 +1
        ENDIF
   10 CONTINUE
      NCH    = 0
      WTOP   = ' '
   15 CONTINUE
*                                                             END CDWTOP
      END
