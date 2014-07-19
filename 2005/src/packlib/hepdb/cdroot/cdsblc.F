*
* $Id: cdsblc.F,v 1.1.1.1 1996/02/28 16:24:32 mclareni Exp $
*
* $Log: cdsblc.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSBLC (PATHN, PATH, NCHAR)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSBLC (PATHN, PATH*, NCHAR*)                           *
*                                                                      *
*   Suppresses the blank characters in a string                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Input character string                                  *
*     PATH     Output character string with all blank characters       *
*              removed                                                 *
*     NCHAR    Number of non-blank characters in PATH                  *
*                                                                      *
*   Called by various routines in the HEPDB package                    *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       PATHN*(*), PATH*(*)
*
*     ------------------------------------------------------------------
*
* *** Suppress blanks from the path name
*
      MAXL   = MIN (MAXLCD, LEN (PATHN)) + 1
      NCHAR  = 0
      ISWT   = 0
      I11    = 1
      DO 10 I = 1, MAXL
        LBLK   = 1
        IF (I.EQ.MAXL) GO TO 5
        IF (PATHN(I:I).NE.' ') LBLK = 0
        IF (ISWT.EQ.0.AND.LBLK.EQ.0) THEN
          ISWT   = 1
          I11    = I
        ENDIF
    5   IF (ISWT.NE.0.AND.LBLK.NE.0) THEN
          IF (NCHAR.EQ.0) THEN
            PATH   = PATHN(I11:I-1)
            NCHAR  = I - I11
          ELSE
            PATH   = PATH(1:NCHAR)//PATHN(I11:I-1)
            NCHAR  = NCHAR + I - I11
          ENDIF
          ISWT   = 0
        ENDIF
   10 CONTINUE
      IF (NCHAR.EQ.0) PATH   = ' '
*                                                             END CDSBLC
      END
