C-      Fetch information from next bank in ISV2 linear structure
C-
C-   Inputs  :
C-     LSUP= supporting link, should be LISAE-IZISV2 or 0 to get information
C-           from first bank in linear structure,
C-           and LISV2 for preceding bank otherwise.
C-   Outputs :
C-     LISV2= structural link to ISV2 providing information
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
      INTEGER LSUP,LISV2,ID,LQISV2,GZISV2
      REAL    P(4),X,Y,Z
C----------------------------------------------------------------------
C
      IF ( LSUP.EQ.0 ) THEN
        LQISV2=GZISV2()
      ELSE
        LQISV2=LQ(LSUP)
      ENDIF
      IF(LQISV2.NE.0) THEN
        ID=IQ(LQISV2+1)
        P(1)=Q(LQISV2+2)
        P(2)=Q(LQISV2+3)
        P(3)=Q(LQISV2+4)
        P(4)=Q(LQISV2+5)
        X=Q(LQISV2+7)
        Y=Q(LQISV2+8)
        Z=Q(LQISV2+9)
      ENDIF
      LISV2=LQISV2
  999 RETURN
      END
