C-      Fetch information from next bank in ISAL linear structure
C-
C-   Inputs  :
C-     LSUP= supporting link, should be LISAE-IZISAL (or 0)
C-           to get information from first bank in linear structure
C-           and LISAL for preceding bank otherwise.
C-   Outputs :
C-     LISAL= structural link to ISAL providing information
C-     ID   = particle ID
C-     P(4) = 4-momentum (px, py, pz, E)
C-     TH   = theta
C-     PHI  = phi
C-     ETA  = eta (pseudo-rapidity)
C-
C-   Created   17-AUG-1989 Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
      INTEGER LSUP,LISAL,ID,LQISAL,GZISAL
      REAL    P(4),TH,PHI,ETA
C----------------------------------------------------------------------
C
      IF(LSUP.EQ.0) THEN
        LQISAL=GZISAL()
      ELSE
        LQISAL=LQ(LSUP)
      ENDIF
      IF(LQISAL.NE.0) THEN
        ID=IQ(LQISAL+1)
        P(1)=Q(LQISAL+2)
        P(2)=Q(LQISAL+3)
        P(3)=Q(LQISAL+4)
        P(4)=Q(LQISAL+5)
        PHI=Q(LQISAL+7)
        TH=Q(LQISAL+8)
        ETA=Q(LQISAL+9)
      ENDIF
      LISAL=LQISAL
  999 RETURN
      END
