*
* $Id: nodeid.F,v 1.1.1.1 1996/02/15 17:47:53 mclareni Exp $
*
* $Log: nodeid.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:53  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      CHARACTER*15 FUNCTION HOSTID()
C
C CERN PROGLIB#    Z313 HOSTID          .VERSION KERNBIT  1.09  920725
C ORIG. 92/07/25, Miguel Marquina
C Extraction of Host Identifier
C
      IMPLICIT NONE

      INCLUDE '($SYIDEF)'
      INTEGER  LIB$GETSYI, ISTAT,ULEN

      CHARACTER*15 STRING
      ISTAT = LIB$GETSYI(SYI$_NODENAME,, STRING, ULEN)
C
      HOSTID=STRING( 1: ULEN)
      RETURN
      END
