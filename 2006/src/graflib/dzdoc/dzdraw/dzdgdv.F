*
* $Id: dzdgdv.F,v 1.1.1.1 1996/03/04 16:13:07 mclareni Exp $
*
* $Log: dzdgdv.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:07  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGDV(CHOPT,IDVERS)
      CHARACTER*(*) CHOPT
      IVPOS=INDEX(CHOPT,'V')
      IF(IVPOS.NE.0)THEN
         IVPOS=IVPOS+1
         IFNUM=INDEXN(CHOPT(IVPOS:))
         IF(IFNUM.NE.1)THEN
            IDVERS=0
         ELSE
            ILNUM=INDXNC(CHOPT(IVPOS:))
            IF(ILNUM.NE.0)THEN
               ILNUM=ILNUM+IVPOS-1
            ELSE
               ILNUM=LEN(CHOPT)
            ENDIF
            CALL DZDCTI(CHOPT(IVPOS:ILNUM),IDVERS)
         ENDIF
       ELSE
         IDVERS=0
       ENDIF
       RETURN
       END
 
