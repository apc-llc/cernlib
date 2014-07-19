C-
C-  INPUT:
C-  PRUNIT = unit number for printout
C-  LISAEI,NISAE,CFL and IFL ignored
C-
C-     SDP  Jan,1986
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
      INTEGER PRUNIT,LISAEI
      INTEGER NISAE,IFL
      CHARACTER CFL*(*)
      INTEGER ID1,ID2,IEVT,NREAC,NJT,NQS,NPJET,NPART,NVERTX,NLEP
      REAL SIGF,WT,QSQ,SHAT,THAT,UHAT
      DOUBLE PRECISION DSEED
      REAL    SSEED(2)
      EQUIVALENCE(DSEED,SSEED(1))
      CHARACTER*8 REACTN(10)
      DATA REACTN/'(TWOJET)','(E+E-)','(DRLYAN)',
     1            '(MBIAS)','(SUSY)','(WPAIR)','(HIGGS)',
     1            '(PHOTON)',' ','(GEANT)'/
C
      IF ( LISAE.GT.0 ) THEN
C
C          Print titles
C
        WRITE(PRUNIT,100)
C
C   Print contents of bank
C
        ID1 = IQ(LISAE+1)
        ID2 = IQ(LISAE+2)
        IEVT = IQ(LISAE+3)
        NREAC = IQ(LISAE+4)
        NJT = IQ(LISAE+5)
        NQS = IQ(LISAE+6)
        NPJET = IQ(LISAE+7)
        NPART = IQ(LISAE+8)
        NVERTX = IQ(LISAE+9)
        NLEP = IQ(LISAE+10)
        SIGF = Q(LISAE+11)
        WT = Q(LISAE+12)
        QSQ = Q(LISAE+13)
        SHAT = Q(LISAE+14)
        THAT = Q(LISAE+15)
        UHAT = Q(LISAE+16)
        SSEED(1)=Q(LISAE+17)
        SSEED(2)=Q(LISAE+18)
        WRITE(PRUNIT,101) NREAC,REACTN(NREAC)
        WRITE(PRUNIT,105) DSEED
        WRITE(PRUNIT,102) ID1,ID2,IEVT
        WRITE(PRUNIT,103) NJT,NQS,NPJET,NPART,NVERTX,NLEP
        WRITE(PRUNIT,104) SIGF,WT,QSQ,SHAT,THAT,UHAT
C
      ENDIF
      RETURN
  100 FORMAT('0',//,1X,80('-'),/,' ISAJET EVENT BANK (ISAE)',/)
  101 FORMAT(/' Reaction = ',I3,2X,A8)
  102 FORMAT(' EVENT ID = ',I10,'-'I10,4X,'EVENT NUMBER = ',I10)
  103 FORMAT(3X,'NISAJ   NISAQ   NPJET   NISP1   NISV1   NISAL',/,7I8)
  104 FORMAT(5X,'SIGMA',6X,'WEIGHT',8X,'Q**2',8X,'SHAT',8X,'THAT',
     $  8X,'UHAT',/,6E12.4)
  105 FORMAT(/,' SEED=',E24.15,/)
      END
