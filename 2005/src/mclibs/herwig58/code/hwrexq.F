*
* $Id: hwrexq.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwrexq.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWREXQ.
*CMZ :-        -02/06/94  11.02.47  by  Mike Seymour
*-- Author :    David Ward, modified by Bryan Webber and Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWREXQ(AV,XMAX)
      DOUBLE PRECISION HWREXQ
C ... Random number from dN/d(x**2)=EXQ(-b*x) with mean AV,
C     But truncated at XMAX
      DOUBLE PRECISION HWRGEN,AV,B,R1,R2,XMAX,R,RMIN
      B=2./AV
      RMIN=EXP(-B*XMAX)
 10   R1=HWRGEN(0)*(1-RMIN)+RMIN
      R2=HWRGEN(1)*(1-RMIN)+RMIN
      R=R1*R2
      IF (R.LT.RMIN) GOTO 10
      HWREXQ=-LOG(R)/B
      END
