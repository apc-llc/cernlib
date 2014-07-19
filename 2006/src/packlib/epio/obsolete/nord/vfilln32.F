*
* $Id: vfilln32.F,v 1.1.1.1 1996/03/08 15:21:58 mclareni Exp $
*
* $Log: vfilln32.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:58  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_ND100B32)||defined(CERNLIB_ND500)
      SUBROUTINE VFILL(IARR,NA,NC)
C. FILL INTEGER ARRAY
      INTEGER*4 IARR(2),NA,NC,ZERO
      DATA ZERO/0/
      IF(NA.LE.ZERO) GOTO 77777
      IA=NA
      DO 1 I=1,IA
    1 IARR(I)=NC
77777 RETURN
      END
#endif
