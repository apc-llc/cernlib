*
* $Id: xzread.F,v 1.1.1.1 1996/03/08 15:44:32 mclareni Exp $
*
* $Log: xzread.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZREAD(LUN,IBUFF,NREC,NWANT,NGOT,CHOPT,IRC)
#include "cspack/zmach.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
#include "cspack/czsock.inc"
      INTEGER IBUFF(NWANT/IQCHAW)
      DIMENSION ICONT(2)
#include "cspack/czopts.inc"
*
      IRC  = 0
      NGOT = 0
*
*     Instruct server to issue a read
*
      CALL CZSWAP(' ',LUN,ISTAT)
      WRITE(CHMAIL,9001) LUN,NREC,NWANT,CHOPT
9001  FORMAT('XZIO :READ',I3,I6,I6,'/',A8,'/')
      CALL CZPUTA(CHMAIL,ISTAT)
*
*     Get status information back from server
*
#include "cspack/czmess.inc"
*
*     Read data and return to caller
*
      IF(IRC.EQ.0) THEN
         ICONT(1) = 0
         LBUF     = NWANT/4
         IF(NGOT.NE.0) LBUF = NGOT
         CALL CZTCP(IBUFF,ICONT)
         ENDIF
      CALL CZSWAP(' ',0,ISTAT)
      IRC = IQUEST(1)
 
      END
 
