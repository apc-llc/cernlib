*
* $Id: streamr.F,v 1.1.1.1 1996/03/08 15:21:50 mclareni Exp $
*
* $Log: streamr.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:50  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_APOLLO)
      SUBROUTINE READ_$STREAM(BUFFER,MAX,RET,STATUS_$RETURNED)
%INCLUDE '/sys/ins/base.ins.ftn'
%INCLUDE '/sys/ins/error.ins.ftn'
%INCLUDE '/sys/ins/streams.ins.ftn'
      INTEGER*4 RET,POINT,STATUS_$RETURNED,KEY(3)
      INTEGER*2 STREAM_$ID,BUFFER(1)
#include "epio/epiocom.inc"
C
C     HERE WE ACTUALLY DO THE READ
C
C--- GET THE STREAM_$ID
      STREAM_$ID = INT2(LIST(ISTART+25))
C--- GO AND READ
      CALL STREAM_$GET_BUF(
     1 STREAM_$ID,
     2  IADDR(BUFFER),
     3   MAX,
     4    IPOINT,
     5     RET,
     6      KEY,
     7       STATUS_$RETURNED)
      IF(STATUS_$RETURNED.NE.STATUS_$OK.AND.
     +ERROR_$CODE(STATUS_$RETURNED).NE.STREAM_$END_OF_FILE)
     +CALL ERROR_$PRINT(STATUS_$RETURNED)
      END
#endif
