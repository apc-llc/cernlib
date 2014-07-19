C-      Fetch information from next bank in ISP2 linear structure
C-
C-   Inputs  :
C-     LSUP= supporting link, should be LISV2-IZISV2 to get information
C-           from first bank in linear structure hanging from vertex
C-           bank ISV2, and LISP2 for preceding bank otherwise.
C-   Outputs :
C-     LISP2= structural link to ISP2 providing information
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
      INTEGER LSUP,LISP2,ID,LQISP2
      REAL    P(4),TH,PHI,ETA
C----------------------------------------------------------------------
#include "isajet/izisp2.inc"
C
      LQISP2=LQ(LSUP)
      IF(LQISP2.NE.0) THEN
        ID=IQ(LQISP2+1)
        P(1)=Q(LQISP2+2)
        P(2)=Q(LQISP2+3)
        P(3)=Q(LQISP2+4)
        P(4)=Q(LQISP2+5)
        PHI=Q(LQISP2+7)
        TH=Q(LQISP2+8)
        ETA=Q(LQISP2+9)
      ENDIF
      LISP2=LQISP2
  999 RETURN
      END
