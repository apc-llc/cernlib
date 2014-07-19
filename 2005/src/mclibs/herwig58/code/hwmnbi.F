*
* $Id: hwmnbi.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwmnbi.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWMNBI.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    David Ward, modified by Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWMNBI(N,AVNCH,EK)
      DOUBLE PRECISION HWMNBI
C---Computes negative binomial probability
      INTEGER N,I
      DOUBLE PRECISION AVNCH,EK,R
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
