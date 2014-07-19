*
* $Id: hwubpr.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwubpr.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWUBPR.
*CMZ :-        -26/04/91  10.18.58  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWUBPR
C     PRINTS OUT DATA ON PARTON SHOWER
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER I,J
      WRITE(6,10) INHAD,XFACT
   10 FORMAT(///10X,'DATA ON LAST PARTON SHOWER:   INHAD =',I3,
     &'    XFACT =',E11.3//'  IPAR ID TM  DA1 CMO AMO CDA ADA',
     &'  P-X     P-Y     P-Z   ENERGY    MASS')
      DO 60 J=1,NPAR
      WRITE(6,50) J,RNAME(ABS(IDPAR(J))),TMPAR(J),JDAPAR(1,J),
     & (JCOPAR(I,J),I=1,4),(PPAR(I,J),I=1,5)
   50 FORMAT(I5,1X,A4,L2,5I4,F7.2,4F8.2)
   60 CONTINUE
      END
