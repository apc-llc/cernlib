*
* $Id: hlccmp.F,v 1.1.1.1 1996/01/16 17:07:41 mclareni Exp $
*
* $Log: hlccmp.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:41  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 18/11/92  18.21.19  by  Rene Brun
*-- Author :
      INTEGER FUNCTION HLCCMP(H1,CH,LGR)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C HLHCMP compare h1 an hollerith chacacter and CH a normal charcater
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C H1  array of integer ( length LGR/4 )
C CH CHARACTER*LGR
C LGR < 40
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER HLHCMP
      INTEGER H1(*)
      CHARACTER CH*(*)
      INTEGER LGR
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER H2(16)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL UCTOH(CH,H2(1),4,LGR)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      HLCCMP = HLHCMP(H1,H2(1),LGR)
      RETURN
C
      END
 
 
