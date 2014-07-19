CDECK  ID>, HWREXT.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    David Ward, modified by Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWREXT(AM0,B)
C-----------------------------------------------------------------------
C     Random number from dN/d(x**2)=exp(-B*TM) distribution, where
C     TM = SQRT(X**2+AM0**2).  Uses Newton's method to solve F-R=0
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWREXT,HWRGEN,AM0,B,R,A,F,DF,DAM,AM
      INTEGER NIT
      EXTERNAL HWRGEN
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
      IF(ABS(DAM).LT..001) GOTO 2
   1  CONTINUE
      CALL HWWARN('HWREXT',1,*2)
   2  HWREXT=AM
      END
