CDECK  ID>, HWRAZM.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWRAZM(PT,PX,PY)
C-----------------------------------------------------------------------
C     RANDOMLY ROTATED 2-VECTOR (PX,PY) OF LENGTH PT
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWRGEN,PT,PX,PY,C,S,CS,QT
      EXTERNAL HWRGEN
   10 C=2.*HWRGEN(1)-1.
      S=2.*HWRGEN(2)-1.
      CS=C*C+S*S
      IF (CS.GT.1. .OR. CS.EQ.0) GOTO 10
      QT=PT/CS
      PX=(C*C-S*S)*QT
      PY=2.*C*S*QT
      END
