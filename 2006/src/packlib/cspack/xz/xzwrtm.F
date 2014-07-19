*
* $Id: xzwrtm.F,v 1.1.1.1 1996/03/08 15:44:32 mclareni Exp $
*
* $Log: xzwrtm.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZWRTM(LUNI,LUNO,IBUFF,NDO,NSTAL,NSTAR,NWRIT,CHOPT,IRC)
#include "cspack/zmach.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
#include "cspack/czsock.inc"
      DIMENSION ICONT(2)
      INTEGER   IBUFF(NWRIT/IQCHAW)
#include "cspack/czopts.inc"
*
      IRC  = 0
*
*     Instruct server to get ready for a write
*
      CALL CZSWAP(' ',LUNO,ISTAT)
      WRITE(CHMAIL,8001) LUNO,NSTAR,NWRIT,NDO,CHOPT
 8001 FORMAT('XZIO :RITM',I3,I6,I6,I6,'/',A8,'/')
      CALL CZPUTA(CHMAIL,ISTAT)
 
      NRECL = NSTAL
 
      DO 33 JJ=1,NDO
 
         IF(IOPTD.NE.0) THEN
            READ(LUNI,REC=NRECL) IBUFF
         ELSE
            READ(LUNI) IBUFF
         ENDIF
         NRECL = NRECL + 1
*
*     Send the data
*
         ICONT(1) = 1
         LBUF     = NWRIT/4
         CALL CZTCP(IBUFF,ICONT)
 
   33 CONTINUE
*
*     Read the return code and return to caller
*
#include "cspack/czmess.inc"
 
      CALL CZSWAP(' ',0,ISTAT)
      IRC = IQUEST(1)
 
      END
 
