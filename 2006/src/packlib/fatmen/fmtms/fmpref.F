*
* $Id: fmpref.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmpref.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPREF(VIP,JP,CHOPT,IRC)
*
*     Convert CHARACTER VID prefix to its INTEGER value
*     or vice-versa depending on CHOPT
*
      CHARACTER*(*) VIP,CHOPT
#include "fatmen/fatvid0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/fatvid1.inc"
#include "fatmen/fatoptc.inc"
      IRC = 0
      IF(IOPTC.NE.0) THEN
         IF((JP.LE.0).OR.(JP.GT.NTMS)) THEN
            IRC = -1
         ELSE
            VIP = PREVID(JP)
         ENDIF
      ELSEIF(IOPTI.NE.0) THEN
         JP = ICNTH(VIP,PREVID,NTMS)
         IF(JP.EQ.0) IRC = -1
      ENDIF
 
      END
