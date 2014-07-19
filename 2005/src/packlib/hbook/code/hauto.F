*
* $Id: hauto.F,v 1.1.1.1 1996/01/16 17:07:31 mclareni Exp $
*
* $Log: hauto.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:31  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.03.23  by  Rene Brun
*-- Author :
      SUBROUTINE HAUTO(Y)
*.==========>
*.           computes automaticaly the bin width during filling
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
      COMMON/HCGARB/DB,ICN,ITAMP,J,NOENT,BFW,IDL,IFW,K,M
     +            ,NWPROX,X0,X1,XCN,GA(3)
*.___________________________________________
      ICN=IQ(LPRX)
      XCN=ICN
      NWPROX=ICN+3
      ITAMP=0
      M=1+I9
      X=Y
      LPX=LCONT+KCON1-1
      NOENT=IQ(LCONT+KNOENT)
      IF(NOENT.GE.3)GO TO 50
      IF(NOENT.EQ.2)GO TO 10
      Q(LPRX+1)=X-0.001*ABS(X)
      IF(X.EQ.0.)Q(LPRX+1)=-1.E-20
      Q(LPRX+2)=X+XCN
      GO TO 999
*
  10  IDL=0
      IF(X.NE.Q(LPRX+1))GO TO 20
      X=1.001*X
  20  IF(X.LT.Q(LPRX))GO TO 30
      Q(LPRX+1)=X+0.001*ABS(X)
      IF(X.EQ.0.)Q(LPRX+1)=1.E+20
      GO TO 40
*
  30  BFW=Q(LPRX+1)
      IDL=1
      Q(LPRX+1)=X-0.001*ABS(X)
      IF(X.EQ.0.)Q(LPRX+1)=-1.E-20
      Q(LPRX+2)=BFW+0.001*ABS(BFW)
      IF(BFW.EQ.0.)Q(LPRX+2)=1.E+20
  40  X0=Q(LPRX+1)
      X1=Q(LPRX+2)
      CALL HBIN(X0,X1,IQ(LPRX),Q(LPRX+1),X1,ICN,DB)
      Q(LPRX+2)=Q(LPRX+1)+XCN*DB
      IF(IDL.EQ.0)GO TO 999
      I=(BFW-Q(LPRX+1))/DB + 2.
      Q(LPX+I)=Q(LPX+2)
      Q(LPX+2)=0.
      IF(I9.EQ.0)GO TO 999
      LW=LQ(LCONT)
      Q(LW+I)=Q(LW+2)
      Q(LW+2)=0.
      GO TO 999
*
  50  CONTINUE
  60  X0=Q(LPRX+1)
      X1=Q(LPRX+2)
      IF(X.LT.X0)GO TO 70
      IF(X.GE.X1)GO TO 90
      GO TO 200
*
  70  Q(LPRX+1)=2.*X0-X1
      IF(ITAMP.NE.1)THEN
         ITAMP=1
         IQ(LCONT+KNOENT)=IQ(LCONT+KNOENT)+1
      ENDIF
      J=ICN+2
      LW=LQ(LCONT)
      DO 80 I=1,ICN,2
         J=J-1
         K=ICN-I+1
         Q(LPX+J)=Q(LPX+K)+Q(LPX+K+1)
         IF(I9.NE.0)Q(LW+J)=Q(LW+K)+Q(LW+K+1)
  80  CONTINUE
*
      CALL VZERO(Q(LPX+2),ICN/2)
      IF(I9.NE.0)CALL VZERO(Q(LW+2),ICN/2)
      GO TO 60
*
  90  Q(LPRX+2)=0.5*(X1+X0)
      IF(ITAMP.NE.1)THEN
         ITAMP=1
         IQ(LCONT+KNOENT)=IQ(LCONT+KNOENT)+1
      ENDIF
      J=1
      LW=LQ(LCONT)
      DO 100 I=1,ICN,2
         J=J+1
         K=I+1
         Q(LPX+J)=Q(LPX+K)+Q(LPX+K+1)
         IF(I9.NE.0)Q(LW+J)=Q(LW+K)+Q(LW+K+1)
 100  CONTINUE
      CALL VZERO(Q(LPX+J+1),ICN/2)
      IF(I9.NE.0)CALL VZERO(Q(LW+J+1),ICN/2)
*
      GO TO 60
*
 200  IF(NOENT/(IQ(LCONT+KNOENT)+1).LT.20)GO TO 999
      IQ(LCONT+KNOENT)=0
      CALL HSIFLA(5,0)
*
 999  RETURN
      END
