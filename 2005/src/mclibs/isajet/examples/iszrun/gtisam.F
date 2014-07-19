C-   Purpose and Methods :
C-      Fetch information from next bank in ISAM linear structure
C-
C-   Inputs  :
C-     LSUP= supporting link, should be LISAE-IZISAM (or 0)
C-           to get information from first bank in linear structure
C-           and LISAM for preceding bank otherwise.
C-   Outputs :
C-     LISAM= structural link to ISAM providing information
C-     ID   = particle ID
C      ITYPE = 0=PROMPT, 1=DECAY, 2=PUNCH
C      ITRIG = 0=DOESN'T PASS MU TRIGGER, 1=CCT, 2=OTC
C      ISPR = SPARE
C-     P(4) = 4-momentum (px, py, pz, E)
C-     TH   = theta
C-     PHI  = phi
C-     ETA  = eta (pseudo-rapidity)
C      SPR  = SPARE
C-
C-   Created   D. HEDIN 15-0CT-90
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
      INTEGER ITYPE,ITRIG,ISPR,LSUP,LISAM,ID,LQISAM,GZISAM
      REAL    P(4),SPR,TH,PHI,ETA
C----------------------------------------------------------------------
C
      IF(LSUP.EQ.0) THEN
        LQISAM=GZISAM()
      ELSE
        LQISAM=LQ(LSUP)
      ENDIF
      IF(LQISAM.NE.0) THEN
        ID=IQ(LQISAM+1)
        ITYPE=IQ(LQISAM+2)
        ITRIG=IQ(LQISAM+3)
        ISPR=IQ(LQISAM+4)
        P(1)=Q(LQISAM+5)
        P(2)=Q(LQISAM+6)
        P(3)=Q(LQISAM+7)
        P(4)=Q(LQISAM+8)
        PHI=Q(LQISAM+9)
        TH=Q(LQISAM+10)
        ETA=Q(LQISAM+11)
        SPR=Q(LQISAM+12)
      ENDIF
      LISAM=LQISAM
  999 RETURN
      END
