C-      Fetch information from next bank in ISV1 linear structure
C-
C-   Inputs  :
C-     LSUP= supporting link, should be LISAE-IZISV1 or 0 to get information
C-           from first bank in linear structure,
C-           and LISV1 for preceding bank otherwise.
C-   Outputs :
C-     LISV1= structural link to ISV1 providing information
C-     ID   = decaying particle ID (1 for primary vertex)
C-     P(4) = 4-momentum (px, py, pz, E)
C-     X,Y,Z   = vertex coordinates
C-
C-   Created   7-DEC-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
      INTEGER LSUP,LISV1,ID,LQISV1,GZISV1
      REAL    P(4),X,Y,Z
C----------------------------------------------------------------------
C
      IF ( LSUP.EQ.0 ) THEN
        LQISV1=GZISV1()
      ELSE
        LQISV1=LQ(LSUP)
      ENDIF
      IF(LQISV1.NE.0) THEN
        ID=IQ(LQISV1+1)
        P(1)=Q(LQISV1+2)
        P(2)=Q(LQISV1+3)
        P(3)=Q(LQISV1+4)
        P(4)=Q(LQISV1+5)
        X=Q(LQISV1+7)
        Y=Q(LQISV1+8)
        Z=Q(LQISV1+9)
      ENDIF
      LISV1=LQISV1
  999 RETURN
      END
