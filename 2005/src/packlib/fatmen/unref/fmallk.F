*
* $Id: fmallk.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fmallk.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMALLK(NEWKEY,IRC)
*
*     Allocate next unused key in CWD
*     Must be a better way of doing this...
*
#include "fatmen/fatpara.inc"
      PARAMETER (MAXKEY=10000)
      PARAMETER (LKEYFA=10)
      COMMON/FAALLK/KEYS
      DIMENSION KEYS(LKEYFA,MAXKEY)
*
      IRC = 0
      CALL RZKEYS(LKEYFA,MAXKEY,KEYS,NKEYS)
*
*     Adding at the end is easy...
*
      IF(NKEYS.EQ.0) THEN
         NEWKEY = 1
         ELSE
*        CALL SORTI(KEYS,LKEYFA,NKEYS,MKSRFA)
*        NEWKEY = KEYS(1,NKEYS) + 1
 
         IBIG = 0
         DO 10 I=1,NKEYS
         IF(KEYS(1,I).GT.IBIG) IBIG = KEYS(1,I)
10       CONTINUE
         NEWKEY = IBIG + 1
         ENDIF
 
      END
 
