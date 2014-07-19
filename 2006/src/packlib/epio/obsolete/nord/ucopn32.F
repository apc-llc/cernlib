*
* $Id: ucopn32.F,v 1.1.1.1 1996/03/08 15:21:58 mclareni Exp $
*
* $Log: ucopn32.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:58  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_ND100B32)||defined(CERNLIB_ND500)
      SUBROUTINE UCOPY(SOURCE,TARGET,NA)
C. COPY NA WORDS FROM SOURCE TO TARGET ARRAY
      INTEGER*4 SOURCE(2),TARGET(2),NA,ZERO
      DATA ZERO/0/
      IF(NA.LE.ZERO) GOTO 77777
      IA=NA
      DO 1 I=1,IA
    1 TARGET(I)=SOURCE(I)
77777 RETURN
      END
#endif
