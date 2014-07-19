#include "isajet/pilot.h"
        REAL FUNCTION SSGWQ2(Q)
C-----------------------------------------------------------------------
C          SSGLBF: glss -> wiss + qk + qb
C          The function phi of PRD36, 96 (1987); Eq. 3.2
C          Modified for non-degenerate squarks 4/6/01
C-----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/sssm.inc"
#include "isajet/sspar.inc"
#include "isajet/sstmp.inc"
        REAL Q,PI
        DOUBLE PRECISION MZ,MG,MS1,MS2,QS,MGS,MS1S,MS2S,MZS,FUN,TERM
        PI=4*ATAN(1.)
        MZ=TMP(1)
        MS1=TMP(2)
        MS2=TMP(3)
        MG=AMGLSS
        QS=Q*Q
        MGS=MG*MG
        MZS=MZ*MZ
        MS1S=MS1*MS1
        MS2S=MS2*MS2
        TERM=(MS2S*MG-2*MS2S*Q-MG*MZS)/(MG-2*Q)/(MS2S-2*MG*Q-MZS)
        IF (TERM.LE.0.D0) THEN
          SSGWQ2=0.
          RETURN
        ELSE
        FUN=(-Q*(MGS-MZS-2*MG*Q)/(MGS-2*Q*MG)-(2*MG*Q-MS2S+MZS)*
     $   DLOG(TERM)/2.D0/MG)/(MGS-MS1S-2*MG*Q)
        SSGWQ2=PI**2*MG*MZ/2.*FUN
        RETURN
        END IF
        END
