*
* $Id: hwuaer.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwuaer.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWUAER.
*CMZ :-        -23/08/94  13.22.29  by  Mike Seymour
*-- Author :    iaN knowles
C-----------------------------------------------------------------------
      FUNCTION HWUAER(R)
C-----------------------------------------------------------------------
C     Real part of photon self-energy: Pi_{gg}(R=M^2/Q^2)
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWUAER,R,ZERO,ONE,TWO,FOUR,FVTHR,THIRD,RMAX,BETA
      PARAMETER (ZERO=0.D0, ONE=1.D0, TWO=2.D0, FOUR=4.D0,
     &           FVTHR=1.666666666666667D0, THIRD=.3333333333333333D0)
      PARAMETER (RMAX=1.D6)
      IF (ABS(R).LT.1.D-3) THEN
C Use assymptotic formula
         HWUAER=-FVTHR-LOG(ABS(R))
      ELSEIF (ABS(R).GT.RMAX) THEN
         HWUAER=ZERO
      ELSEIF (R.GT.0.25) THEN
         BETA=SQRT(FOUR*R-ONE)
         HWUAER=THIRD
     &         -(ONE+TWO*R)*(TWO-BETA*ACOS(ONE-ONE/(TWO*R)))
      ELSE
         BETA=SQRT(ONE-FOUR*R)
         HWUAER=THIRD
     &         -(ONE+TWO*R)*(TWO+BETA*LOG(ABS((BETA-ONE)/(BETA+ONE))))
      ENDIF
      RETURN
      END
