CDECK  ID>, HWDWWT.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWDWWT(EMSQ,A,B,C)
C-----------------------------------------------------------------------
C     MATRIX ELEMENT SQUARED FOR V-A WEAK DECAY
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWDWWT,EMSQ,A,B,C
      HWDWWT=(A-EMSQ)*(EMSQ-B)*C
      END
