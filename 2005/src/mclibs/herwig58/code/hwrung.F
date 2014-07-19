*
* $Id: hwrung.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwrung.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWRUNG.
*CMZ :-        -26/04/91  14.55.45  by  Federico Carminati
*-- Author :    David Ward, modified by Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWRUNG(A,B)
      DOUBLE PRECISION HWRUNG
C ... Random number from distribution having flat top [-A,A] and gaussian
C     tail of s.d. B
      DOUBLE PRECISION ZERO, ONE, TWO, THREE, HALF
      PARAMETER (ZERO=0.D0, ONE=1.D0, TWO=2.D0,
     &           THREE=3.D0, HALF=0.5D0)
      DOUBLE PRECISION HWRGAU,HWRUNI,A,B,PRUN
      LOGICAL HWRLOG
      IF(A.EQ.0) THEN
      PRUN=0
      ELSE
      PRUN=1./(1.+B*1.2533/A)
      ENDIF
      IF(HWRLOG(PRUN)) THEN
      HWRUNG=HWRUNI(0,-A,A)
       ELSE
      HWRUNG=HWRGAU(0,ZERO,B)
      HWRUNG=HWRUNG+SIGN(A,HWRUNG)
       ENDIF
      END
