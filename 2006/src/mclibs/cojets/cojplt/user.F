*
* $Id: user.F,v 1.1.1.1 1996/01/11 14:15:05 mclareni Exp $
*
* $Log: user.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:05  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE USER
C     ***************
C
C          USER ROUTINE TO DO ANALYSIS FOR EACH EVENT.
C
C          ALL HISTOGRAMS SHOULD BE FILLED WITH HFILL USING WEIGHT 1.
C          THE CORRECT WEIGHT WILL BE INSERTED AUTOMATICALLY IN COJPLT.
C
C
C          COJETS COMMON BLOCKS
#include "cojets/idrun.inc"
#include "cojets/itapes.inc"
#include "cojets/jetpar.inc"
#include "cojets/jetset.inc"
#include "cojets/keys.inc"
#include "cojets/partcl.inc"
#include "cojets/pfirst.inc"
#include "cojets/pinits.inc"
#include "cojets/pjets.inc"
#include "cojets/primar.inc"
#include "cojets/rectp.inc"
C
C
C          CALCULATE DESIRED QUANTITIES BELOW AND FILL HISTOGRAMS
C          WITH HFILL.
C-----------------------------------------------------------------------
C-
C
C- PRINT EVENT
      IF(IEVT.LE.2) CALL PRTEVT(0)
C
C- MONITOR PTFFF (FOR TWOJET)
      CALL HFILL(1,PTFFF,0.,1.)
C
C- PARTICLES
      IF(NPTCL.EQ.0) GO TO 10
      TOTET=0.
      DO 1 IPART=1,NPTCL
      IF(IDCAY(IPART).NE.0) GO TO 1
      ET=SQRT(PPTCL(1,IPART)**2+PPTCL(2,IPART)**2+PPTCL(5,IPART)**2)
      TOTET=TOTET+ET
C- SELECT ELECTRON/POSITRON
      ITYPE=ABS(IDENT(IPART))
      IF(ITYPE.EQ.12) CALL HFILL(15,ET,0.,1.)
1     CONTINUE
      CALL HFILL(11,TOTET,0.,1.)
C
10    CONTINUE
C
C- JETS
      IF(NJSET.EQ.0) GO TO 20
      DO 11 JSET=1,NJSET
      IF(JDCAY(JSET).NE.0) GO TO 11
C- REMOVE FINAL TOP PARTONS, RECORDING ONLY TOP DECAY JETS
      IF(ABS(JTYPE(JSET)).EQ.6) GO TO 11
      ETJ=SQRT(PJSET(1,JSET)**2+PJSET(2,JSET)**2)
      Y=SIGN(-LOG(TAN(ATAN(ETJ/ABS(PJSET(3,JSET)))/2.)),PJSET(3,JSET))
      CALL HFILL(21,ETJ,0.,1.)
      CALL HFILL(22,Y,0.,1.)
11    CONTINUE
C
20    CONTINUE
C-
C-----------------------------------------------------------------------
      RETURN
      END
