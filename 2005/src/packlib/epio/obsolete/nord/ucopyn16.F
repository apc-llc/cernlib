*
* $Id: ucopyn16.F,v 1.1.1.1 1996/03/08 15:21:58 mclareni Exp $
*
* $Log: ucopyn16.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:58  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_ND100B16)
      SUBROUTINE UCOPY(SOURCE,TARGET,NA)
C. COPY NA WORDS FROM SOURCE TO TARGET ARRAY
      INTEGER*2 SOURCE(2),TARGET(2),NA,ZERO
      DATA ZERO/0/
      IF(NA.LE.ZERO) GOTO 77777
      DO 1 I=1,NA
    1 TARGET(I)=SOURCE(I)
77777 RETURN
      END
#endif
