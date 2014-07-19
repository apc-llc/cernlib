C-      Fetch information from next bank in ISAQ linear structure
C-
C-   Inputs  :
C-     LSUP= supporting link, should be LISAE-IZISAQ to get information
C-           from first bank in linear structure hanging from vertex
C-           bank ISV1, and LISAQ from preceding bank otherwise.
C-   Outputs :
C-     LISAQ= structural link to ISAQ providing information
C-     ID   = particle ID
C-     P(4) = 4-momentum (px, py, pz, E)
C-     TH   = theta
C-     PHI  = phi
C-     ETA  = eta (pseudo-rapidity)
C-
C-   Created   7-DEC-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
      INTEGER LSUP,LISAQ,ID,LQISAQ,GZISAQ
      REAL    P(4),TH,PHI,ETA
C----------------------------------------------------------------------
C
      IF(LSUP.EQ.0) THEN
        LQISAQ=GZISAQ()
      ELSE
        LQISAQ=LQ(LSUP)
      ENDIF
      IF(LQISAQ.NE.0) THEN
        ID=IQ(LQISAQ+1)
        P(1)=Q(LQISAQ+2)
        P(2)=Q(LQISAQ+3)
        P(3)=Q(LQISAQ+4)
        P(4)=Q(LQISAQ+5)
        PHI=Q(LQISAQ+7)
        TH=Q(LQISAQ+8)
        ETA=Q(LQISAQ+9)
      ENDIF
      LISAQ=LQISAQ
  999 RETURN
      END
