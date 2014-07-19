
C ********************************************************************

      SUBROUTINE LWBB(ENU)

C...Gives energy (ENU) of a (anti-)neutrino chosen from a simple
C...parametrization of a wide band beam.

      DATA EMEAN,SLOPE,EMIN,EMAX/30.,0.02,12.,300./
      A1=1./(EMEAN-12.)
      A2=EXP(EMEAN*SLOPE)
  100 ENU=300.*RLU(0)
      IF(ENU.LT.EMEAN)THEN
      E=A1*(ENU-12.)
      ELSE
      E=A2*EXP(-ENU*SLOPE)
      ENDIF
      IF(ENU.LT.EMIN.OR.ENU.GT.EMAX) GOTO 100
      IF(E.LT.RLU(0)) GOTO 100
      RETURN
      END
