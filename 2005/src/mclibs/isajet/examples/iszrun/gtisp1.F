C-      Fetch information from next bank in ISP1 linear structure
C-
C-   Inputs  :
C-     LSUP= supporting link, should be LISV1-IZISP1 to get information
C-           from first bank in linear structure hanging from vertex
C-           bank ISV1, and LISP1 for preceding bank otherwise.
C-   Outputs :
C-     LISP1= structural link to ISP1 providing information
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
      INTEGER LSUP,LISP1,ID,LQISP1
      REAL    P(4),TH,PHI,ETA
C----------------------------------------------------------------------
#include "isajet/izisp1.inc"
C
      LQISP1=LQ(LSUP)
      IF(LQISP1.NE.0) THEN
        ID=IQ(LQISP1+1)
        P(1)=Q(LQISP1+2)
        P(2)=Q(LQISP1+3)
        P(3)=Q(LQISP1+4)
        P(4)=Q(LQISP1+5)
        PHI=Q(LQISP1+7)
        TH=Q(LQISP1+8)
        ETA=Q(LQISP1+9)
      ENDIF
      LISP1=LQISP1
  999 RETURN
      END
