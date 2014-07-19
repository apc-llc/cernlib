CDECK  ID>, HWREXQ.
*CMZ :-        -02/06/94  11.02.47  by  Mike Seymour
*-- Author :    David Ward, modified by Bryan Webber and Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWREXQ(AV,XMAX)
C-----------------------------------------------------------------------
C     Random number from dN/d(x**2)=EXQ(-b*x) with mean AV,
C     But truncated at XMAX
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWREXQ,HWRGEN,AV,B,BXMAX,R1,R2,XMAX,R,RMIN
      EXTERNAL HWRGEN
      B=2./AV
      BXMAX=B*XMAX
      IF (BXMAX.LT.50) THEN
        RMIN=EXP(-BXMAX)
      ELSE
        RMIN=0
      ENDIF
 10   R1=HWRGEN(0)*(1-RMIN)+RMIN
      R2=HWRGEN(1)*(1-RMIN)+RMIN
      R=R1*R2
      IF (R.LT.RMIN) GOTO 10
      HWREXQ=-LOG(R)/B
      END
