*
* $Id: m443t.F,v 1.1.1.1 1996/02/15 17:47:55 mclareni Exp $
*
* $Log: m443t.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE M443T
      CHARACTER*8 STRING, CHARN
      CALL KBHDR('M443 CHARN')
      STRING='12345'
      IN    =ICHARN(STRING)
      PRINT 1000, STRING, IN
*
      STRING='-67890'
      IN    =ICHARN(STRING)
      PRINT 1000, STRING, IN
*
      STRING='12.345'
      RN    =RCHAR(STRING)
      PRINT 1001, STRING, RN
*
      STRING='-678.90'
      RN    =RCHAR(STRING)
      PRINT 1001, STRING, RN
*
      STRING=CHARN(IN)
      PRINT 1002, STRING, IN
*
      IN=123456789
      STRING=CHARN(IN)
      PRINT 1002, STRING, IN
*
*     STRING=CHARN(RN)
*     PRINT 1003, STRING, RN
*
1000  FORMAT(6X,'CHAR TO I translation: string ',A,' number',I10)
1001  FORMAT(6X,'CHAR TO R translation: string ',A,' number',F10.5)
1002  FORMAT(6X,'I TO CHAR translation: string ',A,' number',I10)
1003  FORMAT(6X,'R TO CHAR translation: string ',A,' number',F10.5)
      RETURN
      END
