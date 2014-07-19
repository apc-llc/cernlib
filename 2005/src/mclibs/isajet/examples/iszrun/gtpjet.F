C-      Fetch information from next bank in PJET linear structure
C-
C-   Inputs  :
C-     LSUP= supporting link, should be 0 to get information
C-           from first bank in linear structure,
C-           and LPJET from preceding bank otherwise.
C-   Outputs :
C-     LPJET= structural link to PJET providing information
C-     ET   = jet transverse energy
C-     P(4) = 4-momentum (px, py, pz, E)
C-     MASS = jet mass
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
#include "isajet/izpjet.inc"
      INTEGER LSUP,LPJET,LQPJET,GZPJET
      REAL    ET,P(4),MASS,TH,PHI,ETA
C----------------------------------------------------------------------
      IF ( LSUP.EQ.0 ) THEN
        LQPJET=GZPJET()
      ELSE
        LQPJET=LQ(LSUP)
      ENDIF
      IF(LQPJET.NE.0) THEN
        ET = Q(LQPJET+2)
        P(1)=Q(LQPJET+3)
        P(2)=Q(LQPJET+4)
        P(3)=Q(LQPJET+5)
        P(4)=Q(LQPJET+6)
        MASS=Q(LQPJET+7)
        PHI=Q(LQPJET+8)
        TH=Q(LQPJET+9)
        ETA=Q(LQPJET+10)
      ENDIF
      LPJET=LQPJET
  999 RETURN
      END
