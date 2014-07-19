*
* $Id: cdconc.F,v 1.1.1.1 1996/02/28 16:24:46 mclareni Exp $
*
* $Log: cdconc.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:46  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCONC (IOT, N, ICON, NSK)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCONC (IOT, N, ICON*, NSK*)                            *
*                                                                      *
*   Routine to calculate the number of concatenated keys               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IOT      I/O Type of the keys                                    *
*     N        Length of the key-vector                                *
*     ICON     Number of concatenated keys to present key              *
*     NSK      Total number of concatenated keys                       *
*                                                                      *
*   Called by CDDISP, CDEDAS, CDJOIN, CDVWPR                           *
*                                                                      *
************************************************************************
*
#include "hepdb/cbconc.inc"
      DIMENSION       ICON(9), IOT(9)
*
*     ------------------------------------------------------------------
*
      CALL VZERO (ICON(1), N)
      NSK  = 0
      I    = 0
      ISYS = 0
      IUSE = 0
    5 CONTINUE
      I    = I + 1
      J    = 0
      IF (I.GT.N) GO TO 999
      IF (IOT(I).EQ.5) THEN
        ISYS = ISYS + 1
        IUSE = IUSE + 1
        IUSECB(ISYS) = IUSE
        ISYSCB(IUSE) = ISYS
   10   K    = I + J + 1
        IF (K.LE.N.AND.IOT(K).EQ.6) THEN
          ISYS = ISYS + 1
          IUSECB(ISYS) = IUSE
          J    = J + 1
          GO TO 10
        ENDIF
        ICON(I) = J
        NSK  = NSK + J
      ELSE
        ISYS = ISYS + 1
        IUSE = IUSE + 1
        IUSECB(ISYS) = IUSE
        ISYSCB(IUSE) = ISYS
      ENDIF
      GO TO 5
*                                                             END CDCONC
  999 END
