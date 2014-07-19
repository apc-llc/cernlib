*
* $Id: hwdthr.F,v 1.1.1.1 1996/03/08 17:02:11 mclareni Exp $
*
* $Log: hwdthr.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWDTHR.
*CMZ :-        -26/04/91  14.55.44  by  Federico Carminati
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWDTHR(P0,P1,P2,P3,WEIGHT)
C     GENERATES THREE-BODY DECAY 0->1+2+3 DISTRIBUTED
C     ACCORDING TO PHASE SPACE * WEIGHT
C------------------------------------------------------------------------
      DOUBLE PRECISION ZERO, ONE, TWO, THREE, HALF
      PARAMETER (ZERO=0.D0, ONE=1.D0, TWO=2.D0,
     &           THREE=3.D0, HALF=0.5D0)
      DOUBLE PRECISION HWRGEN,HWRUNI,A,B,C,D,AA,BB,CC,DD,EE,FF,PP,QQ,WW,
     & RR,PCM1,PC23,WEIGHT,P0(5),P1(5),P2(5),P3(5),P23(5)
      EXTERNAL WEIGHT
      A=P0(5)+P1(5)
      B=P0(5)-P1(5)
      C=P2(5)+P3(5)
      IF (B.LT.C) CALL HWWARN('HWDTHR',100,*999)
      D=ABS(P2(5)-P3(5))
      AA=A*A
      BB=B*B
      CC=C*C
      DD=D*D
      EE=(B-C)*(A-D)
      A=0.5*(AA+BB)
      B=0.5*(CC+DD)
      C=4./(A-B)**2
C
C  CHOOSE MASS OF SUBSYSTEM 23 WITH PRESCRIBED DISTRIBUTION
C
   10 FF=HWRUNI(0,BB,CC)
      PP=(AA-FF)*(BB-FF)
      QQ=(CC-FF)*(DD-FF)
      WW=WEIGHT(FF,A,B,C)**2
      RR=EE*FF*HWRGEN(0)
      IF (PP*QQ*WW.LT.RR*RR) GO TO 10
C
C  FF IS MASS SQUARED OF SUBSYSTEM 23.
C
C  DO 2-BODY DECAYS 0->1+23, 23->2+3
C
      P23(5)=SQRT(FF)
      PCM1=SQRT(PP)*0.5/P0(5)
      PC23=SQRT(QQ)*0.5/P23(5)
      CALL HWDTWO(P0,P1,P23,PCM1,TWO,.TRUE.)
      CALL HWDTWO(P23,P2,P3,PC23,TWO,.TRUE.)
  999 END
