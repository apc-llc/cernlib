*
* $Id: inset.F,v 1.1.1.1 1996/01/11 14:14:39 mclareni Exp $
*
* $Log: inset.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:39  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE INSET(PJET,IFLAJ,QSQJ,IQOR,SETIN)
C     ********************************************
C-- SETS PARAMETERS TO INPUT JETQCD AND TO (SUBSEQUENTLY) ROTATE
C-- QCD SHOWER QUANTA ALONG PJET 3-VECTOR
C--      PJET(4) - 4-MOMENTUM OF INITIATING QUANTUM
C--      IFLAJ   - FLAVOR         "       "
C--      QSQJ    - MASS SQUARE    "       "
C--      IQOR    - EXTERNAL FLAG ATTACHED TO SHOWER QUANTA (GOES
C--                INTO PARQUA(I,7))
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
#include "cojets/qcds.inc"
#include "cojets/quaor.inc"
#include "cojets/rotq.inc"
#include "cojets/transl.inc"
      DIMENSION PJET(4),SETIN(6)
C
      IQUAOR=IQOR
      PJMT2=PJET(1)**2+PJET(2)**2
      PJM=SQRT(PJMT2+PJET(3)**2)
      SETIN(1)=0.
      SETIN(2)=0.
      SETIN(3)=PJET(4)+PJM
      SETIN(5)=QSQJ
      SETIN(6)=IFLAJ
      IF(IFLAJ.EQ.LGLUS.OR.IFLAJ.EQ.0) SETIN(6)=LGLUT
      IF(PJMT2.LT.1.E-30.AND.PJET(3).GE.0.) GO TO 10
      IROT=3
      CT=PJET(3)/PJM
      ST=SQRT(1.-CT**2)
      PJMT=SQRT(PJMT2)
      CPH=PJET(1)/PJMT
      SPH=PJET(2)/PJMT
      RETURN
10    IROT=1
      RETURN
      END
