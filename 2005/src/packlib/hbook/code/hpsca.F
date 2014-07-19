*
* $Id: hpsca.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hpsca.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 05/03/89  09.22.02  by  Rene Brun
*-- Author :
      SUBROUTINE HPSCA(A,N,ICY,Y,FACTOR,XMINI)
*.==========>
*.            THIS ROUTINE PRINTS ONE LINE OF A SCATTER PLOT
*..=========> ( R.Brun )
      DIMENSION A(N)
      COMMON/HFORM/IA(127),IDU
#include "hbook/hcprin.inc"
      COMMON/HEXPO/ISI,IE1,IE2,NBSCAT,FACT
      SAVE ICOL
      DATA ICOL/6/
*.___________________________________________
      KOLD=0
   1  CALL VBLANK(IA,NCOLPA)
      DO 35 I=1,N
         J=XMINI+1.+FACTOR*A(I)
         K=I+ICOL+8
         IF(IH.GT.1)K=K+1
         IF(IH.EQ.1.AND.I.EQ.1)K=K-1
         IF(IH.EQ.NHT.AND.I.EQ.N)K=K+1
         IF(J.EQ.1)THEN
            J=41
            IF(ABS(XMINI-A(I)).GT.0.01/FACTOR)J=38
         ELSE
            IF(J.LT.1)J=41
            IF(J.GE.37)J=37
            IF(J.EQ.2)J=40
            IF(J.EQ.40)GO TO 35
            IF(FACTOR.EQ.1.)THEN
               IF(NBSCAT.LT.32)THEN
                  IF(J.GT.MAXBIT(NBSCAT))J=37
               ENDIF
            ENDIF
         ENDIF
  30     IF(ABS(A(I)).LT.0.01/FACTOR)J=41
  35  IA(K)=IDG(J)
*
*             PRINTS THE LINE
*
      IA(12)=IDG(37)
      K=K+1
      IA(K+1)=IDG(37)
      IF(ICY.EQ.IDG(25))GO TO 50
      IF(ICY.EQ.IDG(31))GO TO 40
      K=K+2
*
      CALL HBCDI(ICY,4,IA(K))
      CALL HBCDF(Y,9,IA)
*
      GO TO 70
*
  40  IA(3)=IDG(31)
      IA(4)=IDG(24)
      IA(5)=IDG(14)
      IA(K+3)=IDG(31)
      IA(K+4)=IDG(24)
      IA(K+5)=IDG(14)
      CALL HFORMA(1)
      CALL VBLANK(IA,NCOLPA)
      J=K+1
      DO 45 I=12,J
  45  IA(I)=IDG(37)
      IF(IE2.EQ.IDG(41))GO TO 70
      IA(1)=IDG(37)
      IA(2)=IDG(2)
      IA(3)=IDG(1)
      IA(4)=IDG(37)
      IA(5)=IDG(37)
      IA(6)=ISI
      IA(7)=IE1
      IA(8)=IE2
      GO TO 70
*
  50  J=K+1
      IF(KOLD.EQ.0)THEN
         DO 55 I=12,J
            IA(I)=IDG(37)
  55     CONTINUE
         CALL HFORMA(1)
         KOLD=1
         GO TO 1
      ENDIF
      IA(3)=IDG(25)
      IA(4)=IDG(32)
      IA(5)=IDG(15)
      IA(K+3)=IDG(25)
      IA(K+4)=IDG(32)
      IA(K+5)=IDG(15)
*
  70  CALL HFORMA(1)
*
      END
