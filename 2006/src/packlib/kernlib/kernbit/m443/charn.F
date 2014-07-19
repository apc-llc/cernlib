*
* $Id: charn.F,v 1.1.1.1 1996/02/15 17:47:50 mclareni Exp $
*
* $Log: charn.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:50  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      CHARACTER*(*) FUNCTION CHARN(N)
*
* CERN PROGLIB# M443    CHARN            .VERSION KERNBIT  1.07  910711
* Author: Miguel Marquina 91/07/11
*
* Build CHARACTER CHARN from INTEGER N
*
* Mods       Date   Comments
*
      CHARACTER*1 NULL,MINUS
      DATA NULL/'0'/,MINUS/'-'/
**
      LN=LEN(CHARN)
      CHARN=' '
      NZERO=ICHAR(NULL)
      NP=1
      IS=ISIGN(1,N)
      NB=ABS(N)
      IP=LN+1
10    CONTINUE
      NDIGIT=MOD(NB,10)
      IP=IP-1
      IF(IP.LT.1) GOTO 20
      CHARN(IP:IP)=CHAR(NDIGIT+NZERO)
      NB=NB/10
      IF(NB.GT.0) GOTO 10
      IF(IS.LT.0) THEN
       IP=IP-1
       IF(IP.LT.1) GOTO 20
       CHARN(IP:IP)=MINUS
      ENDIF
      RETURN
20    CONTINUE
      CHARN='*'
      PRINT 1000,N,LN
1000  FORMAT(' **** M443 CHARN: The number ',I10,' cannot be stored'/
     +       '                   in CHARN (declared length:',I4,')')
      END
