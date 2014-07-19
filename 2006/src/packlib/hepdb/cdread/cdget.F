*
* $Id: cdget.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cdget.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDGET(CHPATH,IXDIV,LSUP,JBIAS,ISEL,KEYS,CHOPT,IRC)
      DIMENSION       ISEL(9), KEYS(9)
      DIMENSION       MASK(100)
      CHARACTER*(*)   CHPATH,CHOPT
#include "hepdb/cdcblk.inc"
      DATA            NENTRY/0/
      SAVE            NENTRY,MASK
*
*     Zero mask vector
*
      IF(NENTRY.EQ.0) THEN
         CALL VZERO(MASK,100)
         NENTRY = 1
      ENDIF
*
*     Retrieve data structure in standard way
*
      CALL CDUSE(CHPATH,LKEYS,ISEL,CHOPT,IRC)
      IF(IRC.EQ.0) THEN
 
         IF(INDEX(CHOPT,'K').EQ.0) THEN
*
*     Shunt data bank over
*
            IFLAG = 1
            LDATA = LQ(LKEYS)
            CALL ZSHUNT(IXDIV,LKEYS,LSUP,JBIAS,IFLAG)
         ENDIF
*
*     Copy keys bank into keys vector
*
         CALL UCOPY(IQ(LKEYS+1),KEYS,IQ(LKEYS-1))
*
*     Free d/s
*
      CALL CDFREE(CHPATH,LKEYS,MASK,KEYS,' ',IRC)
      ENDIF
 
      END
