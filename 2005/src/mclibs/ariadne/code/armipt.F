C***********************************************************************
C $Id: armipt.F,v 1.2 1996/04/10 12:33:23 mclareni Exp $

      REAL FUNCTION ARMIPT(IFST,ILST)

C...ARiadne function determine MInimum PT2

C...Determines the minimum p_t^2 of any gluon between positions 
C...IF and IL.

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "ardat1.f"

      INXT(IP)=IP3(IDO(IP))
      IPRV(IP)=IP1(IDI(IP))


      ARMIPT=PARA(40)
      DO 100 I=IFST,ILST
        IF (.NOT.QQ(I)) THEN
          IF (INXT(I).NE.IPRV(I))
     $         ARMIPT=MIN(ARMIPT,ARIPT2(IPRV(I),I,INXT(I)))
        ENDIF
 100  CONTINUE

      RETURN

C**** END OF ARMIPT ****************************************************
      END
