CDECK  ID>, HWRPIP.
*CMZ :-        -
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWRPIP
C-----------------------------------------------------------------------
C     Generates a random primary IP using a triple Gaussian distribution
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGAU,VIP(3),VIPWID(3)
      INTEGER I
      EXTERNAL HWRGAU
      DATA VIP,VIPWID/3*0.0,.25,.015,1.8/
      DO 10 I=1,3
  10  VTXPIP(I)=HWRGAU(I,VIP(I),VIPWID(I))
      VTXPIP(4)=0.
      END
