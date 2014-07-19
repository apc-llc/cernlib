C-      Fetch information from next bank in ISAJ linear structure
C-
C-   Inputs  :
C-     LSUP= supporting link, should be LISAE-IZISAJ to get information
C-           from first bank in linear structure hanging from vertex
C-           bank ISV1, and LISAJ from preceding bank otherwise.
C-   Outputs :
C-     LISAJ= structural link to ISAJ providing information
C-     ID   = particle ID
C-     P(4) = 4-momentum (px, py, pz, E)
C-     TH   = theta
C-     PHI  = phi
C-     ETA  = eta (pseudo-rapidity)
C-
C-   Created  29-JAN-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
      INTEGER LSUP,LISAJ,ID,LQISAJ,GZISAJ
      REAL    P(4),TH,PHI,ETA
C----------------------------------------------------------------------
C
      IF(LSUP.EQ.0) THEN
        LQISAJ=GZISAJ()
      ELSE
        LQISAJ=LQ(LSUP)
      ENDIF
      IF(LQISAJ.NE.0) THEN
        ID=IQ(LQISAJ+1)
        P(1)=Q(LQISAJ+2)
        P(2)=Q(LQISAJ+3)
        P(3)=Q(LQISAJ+4)
        P(4)=Q(LQISAJ+5)
        PHI=Q(LQISAJ+7)
        TH=Q(LQISAJ+8)
        ETA=Q(LQISAJ+9)
      ENDIF
      LISAJ=LQISAJ
  999 RETURN
      END
