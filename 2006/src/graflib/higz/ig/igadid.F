*
* $Id: igadid.F,v 1.1.1.1 1996/02/14 13:10:31 mclareni Exp $
*
* $Log: igadid.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.15/03 22/07/92  11.29.26  by  O.Couet
*-- Author :    O.Couet   25/05/92
      SUBROUTINE IGADID(LEVEL,N,ID)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hicode.inc"
      DIMENSION ID(*)
*.______________________________________
*
      IF(LPICT.LT.0)RETURN
      IF(IZPUSH(N+1,0,0,'IGADID').NE.0)RETURN
      CALL IZSTCC(IMIDCO,INTPTR)
      IQ(LHI+INTPTR)=LEVEL
      DO 10 I=1,N
         IQ(LHI+INTPTR+I)=ID(I)
   10 CONTINUE
      CALL IZINCI(N+1)
*
      END
