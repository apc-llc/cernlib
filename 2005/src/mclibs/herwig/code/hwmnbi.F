CDECK  ID>, HWMNBI.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    David Ward, modified by Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWMNBI(N,AVNCH,EK)
C-----------------------------------------------------------------------
C---Computes negative binomial probability
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWMNBI,AVNCH,EK,R
      INTEGER N,I
      IF(N.LE.0) THEN
       HWMNBI=0
      ELSE
       R=AVNCH/EK
       HWMNBI=(1.+R)**(-EK)
       R=R/(1.+R)
       DO 1 I=1,N
       HWMNBI=HWMNBI*R*(EK+I-1)/I
    1  CONTINUE
      ENDIF
      END
