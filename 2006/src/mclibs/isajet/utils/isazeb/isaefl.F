*
* $Id: isaefl.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: isaefl.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISAEFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     fill all ISAJET event banks
C-
C-   Created   7-NOV-1988   Serban D. Protopopescu
C-   Modified 15-Jan-1990           "
C-             this version set up for ISAJET, ISAE is head bank
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "idrun.inc"
#include "final.inc"
#include "jetpar.inc"
#include "jetset.inc"
#include "keys.inc"
#include "primar.inc"
#include "isabnk.inc"
#include "izisv1.inc"
#include "zebcom.inc"
#include "seed.inc"
C
      INTEGER QREF(MXJSET),PQREF(5)
      INTEGER I,K,IOISAE,IUH,LISV1,GZISV1
      INTEGER NPART,NVERTX,NQS,NQF,NLEP,NJT,NREAC,NPJET
      INTEGER LISAJ,LISAQ,GZISAQ,GZISAJ,LPJHD,GZPJHD
      INTEGER ISA_RUNNO
      LOGICAL QPART,QCAL,QLEP
      LOGICAL FIRST
      DOUBLE PRECISION DSEED
      REAL SSEED(2)
      EQUIVALENCE(DSEED,SSEED)
C
      LOGICAL EDIT,YES
C----------------------------------------------------------------------
C
      DATA FIRST/.TRUE./
C
      IF(.NOT.EDIT(I)) GOTO 999   ! quit if don't wish to write event
C
      DO 6 I=1,10
        IF(KEYS(I)) NREAC=I
    6 CONTINUE
C
C  create head bank
C
      CALL MZWIPE(0)      ! make sure division is wiped before starting
C
C  create ZEBRA bank ISAE (main supporting bank)
C
      CALL BKISAE
      READ(XSEED,'(E24.15)') DSEED
      Q(LISAE+11)=SIGF*1000.        ! cross section in microbarns
      Q(LISAE+12)=Q(LISAE+11)/NEVENT  ! weight
      Q(LISAE+13)=QSQ               ! effective q**2
      Q(LISAE+14)=SHAT              ! hard scattering invariant s
      Q(LISAE+15)=THAT              !  "      "          "      t
      Q(LISAE+16)=UHAT              !  "      "          "      u
      Q(LISAE+17)=SSEED(1)          !  part 1 of SEED
      Q(LISAE+18)=SSEED(2)          !  part 2 of SEED
      IQ(LISAE+1)=IDG(1)            ! event id
      IQ(LISAE+2)=IDG(2)            !   "
      IQ(LISAE+3)=IEVT              ! event number
      IQ(LISAE+4)=NREAC             ! reaction type
      NPART=0
      NVERTX=0
      NLEP=0
      NPJET=0
C
C  recalculate jet and parton momenta starting from particles
      CALL QRECAL
C
C  create ZEBRA banks for primary partons
      CALL ISAJFL
C
C  create ZEBRA banks ISAQ (initial and final partons)
C
      CALL ISAQFL
C
C  create ISP1 ZEBRA banks (stable particles)
C  and ISV1 ZEBRA banks (short lived vertices)
C
      CALL ISAPFL(NPART,NVERTX)
C
C  create ISAC ZEBRA bank (pseudocalorimeter) if requested
C
      CALL ISLBNK(QPART,QCAL,QLEP)    ! find banks requested
      IF(QCAL) CALL ISACFL
C
C  create ISAL ZEBRA banks (leptons) if requested
C
      IF(QLEP) CALL ISALFL(NLEP)
C
      IF(.NOT.QPART) THEN        ! drop particle and vertex banks
        LISV1=GZISV1()
        CALL MZDROP(IXCOM,LISV1,'L')
        NPART=0
        NVERTX=0
      ENDIF
      CALL PJETFL
      LPJHD=GZPJHD()
      NPJET=IQ(LPJHD+3)
C
C fill rest of ISAE
C
      CALL ISNUMQ(NJT,NQS)      ! find number of primary and secondary partons
C
      IQ(LISAE+5)=NJT               ! number of primary parton banks
      IQ(LISAE+6)=NQS               !  "     of stable parton banks
      IQ(LISAE+7)=NPJET             !  "     of PJET banks
      IQ(LISAE+8)=NPART             !  "     of particle banks
      IQ(LISAE+9)=NVERTX            !  "     of vertex banks
      IQ(LISAE+10)=NLEP             !  "     of lepton banks
  999 RETURN
      END
