*
* $Id: calini.F,v 1.11 2001/10/08 14:03:14 mclareni Exp $
*
* $Log: calini.F,v $
* Revision 1.11  2001/10/08 14:03:14  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE CALINI
C
C          Initialize calorimeter for CALSIM and GETJET.  Note that
C          because the initialization is separate, CALSIM can be
C          called more than once to simulate pileup of several events.
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C          ISAJET common blocks
#include "itapes.inc"
C
C          ISAPLT common blocks
#include "calor.inc"
#include "getjet.inc"
C
      INTEGER IPHI,IY
      REAL PHIX,YX,THX
C          Initialize ET array.
      DO 100 IPHI=1,NCPHI
      DO 100 IY=1,NCY
        ET(IY,IPHI)=0.
        ETEM(IY,IPHI)=0.
100   CONTINUE
C
C          Calculate trig. functions.
      DO 200 IPHI=1,NCPHI
        PHIX=DELPHI*(IPHI-.5)
        CPHCAL(IPHI)=COS(PHIX)
        SPHCAL(IPHI)=SIN(PHIX)
200   CONTINUE
      DO 300 IY=1,NCY
        YX=DELY*(IY-.5)+YCMIN
        THX=2.*ATAN(EXP(-YX))
        CTHCAL(IY)=COS(THX)
        STHCAL(IY)=SIN(THX)
300   CONTINUE
      RETURN
      END
