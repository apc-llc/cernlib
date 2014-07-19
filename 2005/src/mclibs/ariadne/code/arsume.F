C***********************************************************************
C $Id: arsume.F,v 1.2 1996/04/10 12:33:39 mclareni Exp $

      SUBROUTINE ARSUME(NULL,BSX,BSY,BSZ,BSE,BSM,NI,I)

C...ARiadne subroutine SUM Energy and momentum

C...Sum energy and mometum of NI partons in /ARPART/


#include "arimpl.f"
#include "arpart.f"

      DIMENSION I(NI)


      IF (NULL.EQ.0) THEN
        BSX=0.0
        BSY=0.0
        BSZ=0.0
        BSE=0.0
      ENDIF

      DO 100 IJ=1,NI
        II=I(IJ)
        BSX=BSX+BP(II,1)
        BSY=BSY+BP(II,2)
        BSZ=BSZ+BP(II,3)
        BSE=BSE+BP(II,4)
 100  CONTINUE

      B0=0.0
      BSM=SQRT(MAX(B0,BSE**2-BSZ**2-BSY**2-BSX**2))

      RETURN

C**** END OF ARSUME ****************************************************
      END
