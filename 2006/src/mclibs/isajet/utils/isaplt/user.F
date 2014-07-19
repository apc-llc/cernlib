*
* $Id: user.F,v 1.11 2001/10/08 14:03:15 mclareni Exp $
*
* $Log: user.F,v $
* Revision 1.11  2001/10/08 14:03:15  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE USER
C
C          User routine to do analysis for each event.
C
C          All histograms should be booked in SETUP and filled with 
C          HFILL using weight 1. The correct weight SIGF/NEVENT will 
C          be inserted by ISAPLT.
C
C          For a trivial calorimeter simulation with resolutions
C            SIGMA(E)/E = RESEM/SQRT(E)  for electromagnetic
C            SIGMA(E)/E = RESHAD/SQRT(E) for hadronic
C            (RESEM = .15, RESHAD = .35  for uranium calorimeter)
C            (RESEM = .15, RESHAD = .70  for iron calorimeter)
C          use
C
C     CALL CALINI
C     CALL CALSIM(RESEM,RESHAD)
C
C          Then to find jets with a simplified version of the UA1 jet
C          algorithm with jet radius RJET and minimum scalar transverse
C          energy EJCUT (RJET=1., EJCUT=5. for UA1) use
C
C     CALL GETJET(RJET,EJCUT)
C
C          See CALINI, CALSIM, and GETJET for additional comments.
C
C          ISAJET common blocks
#include "itapes.inc"
#include "primar.inc"
#include "pjets.inc"
#include "partcl.inc"
#include "jetset.inc"
#include "idrun.inc"
#include "keys.inc"
#include "final.inc"
C
C          ISAPLT common blocks
#include "calor.inc"
#include "getjet.inc"
C
C          Calculate desired quantities below and fill histograms
C          with HFILL.
C-----------------------------------------------------------------------
C-
C-
C-
C-
C-
C-----------------------------------------------------------------------
      RETURN
      END
