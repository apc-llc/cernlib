
C **********************************************************************

      SUBROUTINE LPRIKT(S,PT,PHI)

C...Size (PT) and azimuthal angle (PHI) of primordial kt according
C...to a Gaussian distribution.

      PT=S*SQRT(-ALOG(RLU(0)))
      PHI=6.2832*RLU(0)
      RETURN
      END
