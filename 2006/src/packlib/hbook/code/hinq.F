*
* $Id: hinq.F,v 1.1 1997/01/29 09:09:10 couet Exp $
*
* $Log: hinq.F,v $
* Revision 1.1  1997/01/29 09:09:10  couet
* - New routine to inquire if a LUN is used by HBOOK or not.
*
*
#include "hbook/pilot.h"
*-- Author : Olivier Couet
      LOGICAL FUNCTION HINQ(LUN)
*.==========>
*.           Returns .TRUE. if the LUN is used by HBOOK, .FALSE. otherwise.
*..=========> 
#include "hbook/hcdire.inc"
*.___________________________________________
*
      HINQ=.FALSE.
      DO 10 I=2,NCHTOP
         IF(ICHTOP(I).EQ.LUN)THEN
            HINQ=.TRUE.
            RETURN
         ENDIF
  10  CONTINUE
*
      END
