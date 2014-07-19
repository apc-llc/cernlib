*
* $Id: hwrext.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwrext.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWREXT.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    David Ward, modified by Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWREXT(AM0,B)
      DOUBLE PRECISION HWREXT
C ... Random number from dN/d(x**2)=exp(-B*TM) distribution, where
C     TM = SQRT(X**2+AM0**2)
C     Uses Newton's method to solve F-R=0
      INTEGER NIT
      DOUBLE PRECISION HWRGEN,AM0,B,R,A,F,DF,DAM,AM
      R=HWRGEN(0)
C --- Starting value
      AM=AM0-LOG(R)/B
      DO 1 NIT=1,20
      A=EXP(-B*(AM-AM0))/(1.+B*AM0)
      F=(1.+B*AM)*A-R
      DF=-B**2*AM*A
      DAM=-F/DF
      AM=AM+DAM
      IF(AM.LT.AM0) AM=AM0+.001
      IF(ABS(DAM).LT..001) GO TO 2
   1  CONTINUE
      CALL HWWARN('HWREXT',1,*2)
   2  HWREXT=AM
      END
