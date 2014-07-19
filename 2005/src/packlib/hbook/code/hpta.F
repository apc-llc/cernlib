*
* $Id: hpta.F,v 1.1.1.1 1996/01/16 17:07:47 mclareni Exp $
*
* $Log: hpta.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.52  by  Rene Brun
*-- Author :
      SUBROUTINE HPTA(C,N,ICY,Y)
*.==========>
*.            THIS ROUTINE PRINTS ONE LINE OF A TABLE
*..=========> ( R.Brun )
      DIMENSION C(N)
      COMMON/HFORM/IA(127),IDU
#include "hbook/hcprin.inc"
      COMMON/HEXPO/ISI,IE1,IE2,NBSCAT,FACT
*.___________________________________________
*
      KOLD=0
   1  CALL VBLANK(IA,NCOLPA)
      DO 10 I=1,N
         K=(I-1)*MSTEP+15
         IF(IH.GT.1)K=I*MSTEP+15
         IF(NBSCAT.GE.32)K=K+4
         IF(IH.EQ.1.AND.I.EQ.1)K=K-1
         IF(IH.EQ.NHT.AND.I.EQ.N)K=K+1
         IF(C(I).EQ.0.)GO TO 10
         IF(NBSCAT.GE.32)THEN
            CALL HBCDF(C(I),9,IA(K))
         ELSE
            IC=C(I)
            CALL HBCDI(IC,MSTEP,IA(K))
         ENDIF
  10  CONTINUE
*
*             PRINTS THE LINE
*
      K=K+MSTEP+3
      IA(12)=IDG(37)
      IA(K-2)=IDG(37)
      IF(ICY.EQ.IDG(25))GO TO 30
      IF(ICY.EQ.IDG(31))GO TO 20
*
      CALL HBCDI(ICY,4,IA(K-1))
      CALL HBCDF(Y,9,IA)
*
      GO TO 50
*
  20  IA(3)=IDG(31)
      IA(4)=IDG(24)
      IA(5)=IDG(14)
      IA(K)=IDG(31)
      IA(K+1)=IDG(24)
      IA(K+2)=IDG(14)
      CALL HFORMA(1)
      CALL VBLANK(IA,NCOLPA)
      J=K-2
      DO 25 I=12,J
  25  IA(I)=IDG(37)
      IF(IE2.EQ.IDG(41))GO TO 50
      IA(1)=IDG(37)
      IA(2)=IDG(2)
      IA(3)=IDG(1)
      IA(4)=IDG(37)
      IA(5)=IDG(37)
      IA(6)=ISI
      IA(7)=IE1
      IA(8)=IE2
      GO TO 50
*
  30  J=K-2
      IF(KOLD.EQ.0)THEN
         DO 35 I=12,J
            IA(I)=IDG(37)
  35     CONTINUE
         CALL HFORMA(1)
         KOLD=1
         GO TO 1
      ENDIF
      IA(3)=IDG(25)
      IA(4)=IDG(32)
      IA(5)=IDG(15)
      IA(K)=IDG(25)
      IA(K+1)=IDG(32)
      IA(K+2)=IDG(15)
*
  50  CALL HFORMA(1)
*
      END
