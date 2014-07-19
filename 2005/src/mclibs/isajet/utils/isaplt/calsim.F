*
* $Id: calsim.F,v 1.11 2001/10/08 14:03:14 mclareni Exp $
*
* $Log: calsim.F,v $
* Revision 1.11  2001/10/08 14:03:14  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE CALSIM(RESEM,RESHAD)
C
C          Trivial calorimeter simulation. Deposit energy 
C            ET(IY,IPHI)   = total energy in bin (IY,IPHI)
C            ETEM(IY,IPHI) = electromagnetic energy in bin (IY,IPHI)
C          in cells with uniform Y and PHI bins and energy resolutions
C            SIGMA/E=RESEM/SQRT(E)  for electrons and photons
C            SIGMA/E=RESHAD/SQRT(E) for hadrons
C          and with no shower spreading. Note that muons deposit their
C          full energy with hadronic resolution.
C
C          Ver 7.33: Treat gravitino = 91 as non-interacting.
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C          ISAJET common blocks
#include "itapes.inc"
#include "partcl.inc"
C
C          ISAPLT common blocks
#include "calor.inc"
#include "getjet.inc"
C
      INTEGER IP,IY,IPHI,IDABS
      REAL PI,PTIP,PIP,PPLUS,PMINUS,YIP,PHIIP,EIP,RGEN1,RGEN2,RANF,R1,
     $RESEM,RESHAD
C
C          Fill calorimeter
C
      PI=4.*ATAN(1.)
      DO 200 IP=1,NPTCL
        IF(IDCAY(IP).NE.0) GO TO 200
        IDABS=IABS(IDENT(IP))
        IF(IDABS.EQ.11.OR.IDABS.EQ.13.OR.IDABS.EQ.15.OR.IDABS.EQ.30
     $  .OR.IDABS.EQ.91) GO TO 200
        PTIP=SQRT(PPTCL(1,IP)**2+PPTCL(2,IP)**2)
        PIP=SQRT(PTIP**2+PPTCL(3,IP)**2)
        PPLUS=PPTCL(4,IP)+PPTCL(3,IP)
        PMINUS=PPTCL(4,IP)-PPTCL(3,IP)
        IF(PPLUS.LE.0..OR.PMINUS.LE.0.) GO TO 200
        YIP=.5*ALOG(PPLUS/PMINUS)
        PHIIP=ATAN2(PPTCL(2,IP),PPTCL(1,IP))
        IF(PHIIP.LT.0.) PHIIP=PHIIP+2.*PI
        IF(YIP.LT.YCMIN.OR.YIP.GT.YCMAX) GO TO 200
        IY=INT((YIP-YCMIN)/DELY)+1
        IPHI=INT(PHIIP/DELPHI)+1
        EIP=PPTCL(4,IP)
C
C          Smear energy
        RGEN1=SQRT(-2.*ALOG(RANF()))
        RGEN2=2.*PI*RANF()
        R1=RGEN1*COS(RGEN2)
        IF(IDABS.EQ.10.OR.IDABS.EQ.12.OR.IDABS.EQ.110.OR.
     $  IDABS.EQ.220) THEN
          EIP=EIP+(RESEM*SQRT(EIP))*R1
          ET(IY,IPHI)=ET(IY,IPHI)+EIP*STHCAL(IY)
          ETEM(IY,IPHI)=ETEM(IY,IPHI)+EIP*STHCAL(IY)
        ELSE
          EIP=EIP+(RESHAD*SQRT(EIP))*R1
          ET(IY,IPHI)=ET(IY,IPHI)+EIP*STHCAL(IY)
        ENDIF
200   CONTINUE
      RETURN
      END
