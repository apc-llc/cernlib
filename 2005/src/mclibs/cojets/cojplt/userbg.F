*
* $Id: userbg.F,v 1.1.1.1 1996/01/11 14:15:05 mclareni Exp $
*
* $Log: userbg.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:05  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE USERBG
C     *****************
C
C          HANDLES INFORMATION FROM BEGINNING RECORD
C
C          COJETS COMMON BLOCKS
#include "cojets/data1.inc"
#include "cojets/decpar.inc"
#include "cojets/dylim.inc"
#include "cojets/fstate.inc"
#include "cojets/idrun.inc"
#include "cojets/itapes.inc"
#include "cojets/jetlim.inc"
#include "cojets/keys.inc"
#include "cojets/mb.inc"
#include "cojets/par.inc"
#include "cojets/primar.inc"
#include "cojets/qcdpar.inc"
#include "cojets/zwpar.inc"
C
C          WRITE HERE INFORMATION FROM BEGINNING RECORD
C-----------------------------------------------------------------------
C-
C          PRINT RUN ID, REAC
      WRITE(ITLIS,10) REAC,IDG
10    FORMAT(//1X,'REACTION ',A8,5X,'RUN ID',2I10/1X,48('='))
      WRITE(ITLIS,11) (I,I=1,10),KEYS
11    FORMAT(/1X,'KEYS',3X,10I2/8X,10L2)
      WRITE(ITLIS,12) ECM,NFLAVR,FACTRK,NEVENT,ALAM,QZEVOL
     *,PTCUTF,PTSQIN,CUTJET,ISTRUC
12    FORMAT(/1X,'ECM =',F8.0,5X,'NFLAVR =',I3
     *,5X,'FACTRK =',F6.2,5X,'NEVENT =',I10
     *//1X,'ALAM =',F7.3,5X,'QZEVOL =',F10.2,5X,'PTCUTF =',F10.2
     *,5X,'PTSQIN =',F8.3,5X,'CUTJET =',F10.2,5X,'ISTRUC =',I3)
      IF(KEYS(4)) WRITE(ITLIS,22) SIGTIN
22    FORMAT(/1X,'SIGTIN =',F8.2)
      IF(KEYS(1)) WRITE(ITLIS,13) PTFFFM,PTFFFX
13    FORMAT(/1X,'PTFFFM =',F10.2,5X,'PTFFFX =',F10.2)
      IF(KEYS(3)) WRITE(ITLIS,14) WTYPE,KWMODE,KYWOPT,KYWQED
14    FORMAT(/1X,'WTYPE  =',2X,A2,5X,'KWMODE =',I4
     *,5X,'KYWOPT =',I4,5X,'KYWQED =',I4)
      IF(KEYS(3).AND.WTYPE.NE.'GM') WRITE(ITLIS,25) WZMINM
25    FORMAT(/1X,'WZMINM =',F10.2)
      IF(KEYS(3).AND.WTYPE.EQ.'GM') WRITE(ITLIS,23) QMIN,QMAX
23    FORMAT(/1X,'QMIN =',F10.2,5X,'QMAX =',F10.2)
      IF(KEYS(1)) WRITE(ITLIS,15) KYHEAV,KYFLAV
15    FORMAT(/1X,'KYHEAV =',I4,5X,'KYFLAV =',I4)
      WRITE(ITLIS,16) KYEVOL,KYTQED,KYTQCD,KTPFRM
16    FORMAT(/1X,'KYEVOL =',I4,5X,'KYTQED =',I4,5X,'KYTQCD =',I4
     *,5X,'KTPFRM =',I4)
      WRITE(ITLIS,17) NODECY,NOFRGM,KFORSL,LFORSL,NFORSL,NFORCE,NSTOPD
17    FORMAT(/1X,'NODECY =',L2,5X,'NOFRGM =',L2,5X,'KFORSL =',I4
     *,5X,'LFORSL =',I4,5X,'NFORSL =',I4,5X,'NFORCE =',I4,5X,'NSTOPD ='
     *,I4)
      WRITE(ITLIS,24) QMAS(6)
24    FORMAT(/1X,'TOP QUARK MASS =',F10.2)
      IF(NFORCE.GT.0) THEN
        WRITE(ITLIS,18)
18      FORMAT(/1X,'KFORCE: DECAY PARTICLE/SECONDARIES')
        WRITE(ITLIS,19) ((KFORCE(J,I),J=1,6),I=1,NFORCE)
19      FORMAT(1X,6I10)
      ENDIF
      IF(NSTOPD.GT.0) THEN
        WRITE(ITLIS,20)
20      FORMAT(/1X,'KSTOPD: STOPPED PARTICLE DECAYS')
        WRITE(ITLIS,21) (KSTOPD(I),I=1,NSTOPD)
21      FORMAT(1X,8I10)
      ENDIF
      WRITE(ITLIS,30)
30    FORMAT(/////)
C-
C-----------------------------------------------------------------------
C
      RETURN
      END
