*
* $Id: fmcp.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmcp.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMCP(GENAMA,GENAMB,IRC)
      CHARACTER*(*) GENAMA,GENAMB
#include "fatmen/faust.inc"
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      CALL FMGET(GENAMA,L,KEYS,IRC)
      IF(IRC.NE.0) RETURN
      CALL FMPUT(GENAMB,L,IRC)
      IF(IRC.NE.0) RETURN
      NFCPFL = NFCPFL + 1
      END
 
