*
* $Id: fpvset.F,v 1.3 1996/05/31 16:24:45 couet Exp $
*
* $Log: fpvset.F,v $
* Revision 1.3  1996/05/31 16:24:45  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:07:12  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  23.16.01  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPVSET(XNAM,YNAM,EYNAM,IERFLG)
      CHARACTER*(*) XNAM,YNAM,EYNAM
      INTEGER IERFLG
*.===========> Author: G. Kozlovsky, 1994
*.
*. Set given vectors as the current vectors for vector/fit command
*..==========>
*
#include "hbook/hcfit2.inc"
#include "hbook/hcfit6.inc"
#include "hbook/hcbook.inc"
#include "paw/quest.inc"
#include "paw/fpcoms.inc"
*
      IERFLG = 0
*
* find XNAM vector
      CALL CLTOU(XNAM)
      CALL KUVECT(XNAM,LX1,LX2)
      IF (LX1 .EQ. 0) THEN
          IERFLG = 2
          RETURN
      ELSE IF (IQUEST(14) .NE. 1) THEN
          IERFLG = 1
          RETURN
      ENDIF
*
* find YNAM vector
      CALL CLTOU(YNAM)
      CALL KUVECT(YNAM,LY1,LY2)
      IF (LY1 .EQ. 0) THEN
          IERFLG = 2
          RETURN
      ELSE IF (IQUEST(14) .NE. 1) THEN
          IERFLG = 1
          RETURN
      ENDIF

*
*
* find EYNAM vector which may be not specified
      CALL CLTOU(EYNAM)
      CALL KUVECT(EYNAM,LEY1,LEY2)
      IF (IQUEST(14) .NE. 1) THEN
          IERFLG = 1
          RETURN
      ENDIF
*
* Compute common length of vectors
      N = MIN(LX2-LX1,LY2-LY1)
      IF (LEY1 .GT. 0) N = MIN(N,LEY2-LEY1)
      N = N + 1
      NX = 1
      NY = N
      NUMEP=N
*
      IWEIGH = 1
      IF (LEY1 .GT. 0) THEN
          IWEIGH = 0
      ENDIF
*
*         Set addresses of working vectors
*
      LAHFIT=NX*NY+2*NY+100
      CALL HWORK(LAHFIT,ILXE,'HFITV')
      IF(ILXE.EQ.0) THEN
          IERFLG = 1
          RETURN
      ENDIF
      ILYE=ILXE+NX*NY
      ILEY=ILYE+NY
*
*
*         Computes ALLCHA and WGTMAX
*
      BINWID=1.
      ALLCHA=0.
      WGTMAX=0.
      DO 10 L1=1,N
         IF (ABS(Q(LY1+L1-1)).GT.WGTMAX) WGTMAX=ABS(Q(LY1+L1-1))
   10 CONTINUE
*
      DO 30 I=1,NX
       DO 20 J=1,NY
           Q(ILXE+J-1+NY*(I-1))=Q(LX1+J-1)
   20   CONTINUE
   30 CONTINUE
      DO 40 I=1,N
        Q(ILYE+I-1)=Q(LY1+I-1)
        IF(IWEIGH.EQ.0)THEN
           Q(ILEY+I-1)=Q(LEY1+I-1)
        ELSE
           Q(ILEY+I-1)=1.
        ENDIF
   40 CONTINUE

      XVECNAM = XNAM
      YVECNAM = YNAM
      EYVECNAM = EYNAM

      END
