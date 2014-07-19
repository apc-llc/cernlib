C-   Purpose and Methods :
C-      Fetch information from next bank in ISCL linear structure
C-
C-   Inputs  :
C-     LSUP= supporting link, should be LISAE-IZISCL to get information
C-           from first bank in linear structure hanging from vertex
C-           bank ISV1, and LISCL from preceding bank otherwise.
C-   Outputs :
C-     LISCL= structural link to ISCL providing information
C-     EEM  = e.m. energy
C-     EHAD = hadronic energy
C-     SNTH = sin(theta)
C-     CSTH = cos(theta)
C-     SNPHI= sin(phi)
C-     CSPHI= cos(phi)
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
      INTEGER LSUP,LISCL,LQISCL,GZISCL
      REAL    EEM,EHAD,SNTH,CSTH,SNPHI,CSPHI,PHI,ETA
C----------------------------------------------------------------------
#include "isajet/iziscl.inc"
C
      IF ( LSUP.EQ.0 ) THEN
        LQISCL=GZISCL()
      ELSE
        LQISCL=LQ(LSUP)
      ENDIF
      IF(LQISCL.NE.0) THEN
        EEM=Q(LQISCL+3)
        EHAD=Q(LQISCL+4)
        SNTH=Q(LQISCL+5)
        CSTH=Q(LQISCL+6)
        SNPHI=Q(LQISCL+7)
        CSPHI=Q(LQISCL+8)
        PHI=Q(LQISCL+9)
        ETA=Q(LQISCL+10)
      ENDIF
      LISCL=LQISCL
  999 RETURN
      END
