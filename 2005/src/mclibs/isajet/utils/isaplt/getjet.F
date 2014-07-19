*
* $Id: getjet.F,v 1.11 2001/10/08 14:03:15 mclareni Exp $
*
* $Log: getjet.F,v $
* Revision 1.11  2001/10/08 14:03:15  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE GETJET(RJET,EJCUT)
C
C          Simple jet-finding algorithm (similar to UA1).
C          Find highest remaining cell > ETSTOP and sum surrounding
C          cells with--
C            DELTA(Y)**2+DELTA(PHI)**2<RJET**2
C            ET>ECCUT.
C          Keep jets with ET>EJCUT.
C          The UA1 parameters are RJET=1.0 and EJCUT=5.0
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C
C          ISAPLT common blocks
#include "itapes.inc"
#include "calor.inc"
#include "getjet.inc"
C
      INTEGER IPHI,IY,J,K,NPHI1,NPHI2,NY1,NY2,IPASS,IYMX,IPHIMX,IPHI1,
     $IPHIX,IY1,IYX
      REAL    RJET,ETMAX,ETSTOP,RR,ECCUT,PX,EJCUT
C          Parameters
      DATA ECCUT/.5/
      DATA ETSTOP/1./
C
C          Initialize
C
      DO 100 IPHI=1,NCPHI
      DO 100 IY=1,NCY
100   JETNO(IY,IPHI)=0
      DO 110 J=1,NJMAX
        ETJET(J)=0.
        DO 110 K=1,4
111     PCJET(K,J)=0.
110   CONTINUE
      NCJET=0
      NPHI1=RJET/DELPHI
      NPHI2=2*NPHI1+1
      NY1=RJET/DELY
      NY2=2*NY1+1
      IPASS=0
C
C          Find highest cell remaining
C
1     ETMAX=0.
      DO 200 IPHI=1,NCPHI
        DO 210 IY=1,NCY
          IF(JETNO(IY,IPHI).NE.0) GO TO 210
          IF(ET(IY,IPHI).LT.ETMAX) GO TO 210
          ETMAX=ET(IY,IPHI)
          IYMX=IY
          IPHIMX=IPHI
210     CONTINUE
200   CONTINUE
      IF(ETMAX.LT.ETSTOP) RETURN
C
C          Sum cells
C
      IPASS=IPASS+1
      IF(IPASS.GT.NCY*NCPHI) THEN
        WRITE(ITLIS,8888) IPASS
8888    FORMAT(//' ERROR IN GETJET...IPASS > ',I6)
        STOP 99
      ENDIF
      NCJET=NCJET+1
      IF(NCJET.GT.NJMAX) THEN
        WRITE(ITLIS,9999) NCJET
9999    FORMAT(//' ERROR IN GETJET...NCJET > ',I5)
        STOP 99
      ENDIF
      DO 300 IPHI1=1,NPHI2
        IPHIX=IPHIMX-NPHI1-1+IPHI1
        IF(IPHIX.LE.0) IPHIX=IPHIX+NCPHI
        IF(IPHIX.GT.NCPHI) IPHIX=IPHIX-NCPHI
        DO 310 IY1=1,NY2
          IYX=IYMX-NY1-1+IY1
          IF(IYX.LE.0) GO TO 310
          IF(IYX.GT.NCY) GO TO 310
          IF(JETNO(IYX,IPHIX).NE.0) GO TO 310
          RR=(DELY*(IY1-NY1-1))**2+(DELPHI*(IPHI1-NPHI1-1))**2
          IF(RR.GT.RJET**2) GO TO 310
          IF(ET(IYX,IPHIX).LT.ECCUT) GO TO 310
          PX=ET(IYX,IPHIX)/STHCAL(IYX)
C          Add cell to jet
          PCJET(1,NCJET)=PCJET(1,NCJET)+PX*STHCAL(IYX)*CPHCAL(IPHIX)
          PCJET(2,NCJET)=PCJET(2,NCJET)+PX*STHCAL(IYX)*SPHCAL(IPHIX)
          PCJET(3,NCJET)=PCJET(3,NCJET)+PX*CTHCAL(IYX)
          PCJET(4,NCJET)=PCJET(4,NCJET)+PX
          ETJET(NCJET)=ETJET(NCJET)+ET(IYX,IPHIX)
          JETNO(IYX,IPHIX)=NCJET
310     CONTINUE
300   CONTINUE
C
C          Discard jet if ET < EJCUT.
C
      IF(ETJET(NCJET).GT.EJCUT) GO TO 1
      ETJET(NCJET)=0.
      DO 400 K=1,4
400   PCJET(K,NCJET)=0.
      NCJET=NCJET-1
      GO TO 1
C
      END
