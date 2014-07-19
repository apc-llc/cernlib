*
* $Id: cdftio.F,v 1.1.1.1 1996/02/28 16:24:50 mclareni Exp $
*
* $Log: cdftio.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:50  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFTIO (IODV, CHIDH)
*     ===============================
*
************************************************************************
*                                                                      *
*        SUBR. CDFTIO (IODV, CHIDH*)                                   *
*                                                                      *
*   Routine to fetch the I/O characteristic of a bank from             *
*   the IODV words into the character string CHIDH                     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IODV     I/O word of the bank                                    *
*     CHIDH    I/O characteristics of the bank                         *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
************************************************************************
*
#include "zebra/zbcd.inc"
#include "zebra/zunit.inc"
#include "hepdb/quest.inc"
#include "zebra/mzioc.inc"
C-------------------------  CDE  -----------------------------------
 
      CHARACTER       CHIDH*(*)
      DIMENSION       IODV(20), MTR(500), INV(7)
      DATA  INV       / 2, 9, 6, 4, 8, 24, 19 /
*
      CALL MZIOCR (IODV)
*
      JT = 1
   21 IF (JFOCUR.EQ.JFOREP)  THEN
        MTR(JT)   = IQSLAS
        MTR(JT+1) = 0
        JT = JT + 2
      ENDIF
*
      JF = MFO(JFOCUR+1)
      JC = MFO(JFOCUR+2)
*
      IF    (JC)             24, 25, 26
   24 MTR(JT) = IQMINS
      JT = JT + 1
      GO TO 28
*
   25 MTR(JT) = IQSTAR
      JT = JT + 1
      GO TO 28
*
   26 CALL USET (JC,MTR(JT),1,6)
      JT = JT + 6
*
   28 JF = INV(JF)
      MTR(JT)   = IQLETT(JF)
      MTR(JT+1) = 0
      JT = JT + 2
 
      JFOCUR = JFOCUR + 2
      IF (JFOCUR.LT.JFOEND)  GO TO 21
*
      JT = JT - 2
      CALL ULEFT (MTR,1,JT)
      NT = IULAST (IQBLAN,MTR,JT)
*
      DO 33 JT=1,NT
      IF (MTR(JT).NE.0)      GO TO 33
      MTR(JT) = IQBLAN
   33 CONTINUE
      CALL UH1TOC (MTR, CHIDH, NT)
*                                                             END CDFTIO
   99 END
