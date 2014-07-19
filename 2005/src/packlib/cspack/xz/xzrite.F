*
* $Id: xzrite.F,v 1.1.1.1 1996/03/08 15:44:32 mclareni Exp $
*
* $Log: xzrite.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZRITE(LUN,IBUFF,NREC,NWRIT,CHOPT,IRC)
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
      CALL CZSWAP(' ',LUN,ISTAT)
      WRITE(CHMAIL,9001) LUN,NREC,NWRIT,CHOPT
9001  FORMAT('XZIO :RITE',I3,I6,I6,'/',A8,'/')
      CALL CZPUTA(CHMAIL,ISTAT)
*
*     Send the data
*
      ICONT(1) = 1
      LBUF     = NWRIT/4
      CALL CZTCP(IBUFF,ICONT)
*
*     Read the return code and return to caller
*
#include "cspack/czmess.inc"
 
      CALL CZSWAP(' ',0,ISTAT)
      IRC = IQUEST(1)
 
      END
 
