*
* $Id: hwrazm.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwrazm.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWRAZM.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWRAZM(PT,PX,PY)
C ... RANDOMLY ROTATED 2-VECTOR (PX,PY) OF LENGTH PT
      DOUBLE PRECISION HWRGEN,PT,PX,PY,C,S,CS,QT
   10 C=2.*HWRGEN(1)-1.
      S=2.*HWRGEN(2)-1.
      CS=C*C+S*S
      IF (CS.GT.1. .OR. CS.EQ.0) GO TO 10
      QT=PT/CS
      PX=(C*C-S*S)*QT
      PY=2.*C*S*QT
      END
