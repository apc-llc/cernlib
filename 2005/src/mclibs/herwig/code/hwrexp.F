CDECK  ID>, HWREXP.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    David Ward, modified by Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWREXP(AV)
C-----------------------------------------------------------------------
C     Random number from dN/d(x**2)=exp(-b*x) with mean AV
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWREXP,HWRGEN,AV,B,R1,R2
      EXTERNAL HWRGEN
      B=2./AV
      R1=HWRGEN(0)
      R2=HWRGEN(1)
      HWREXP=-LOG(R1*R2)/B
      END
