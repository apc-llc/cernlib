*
* $Id: streamw.F,v 1.1.1.1 1996/03/08 15:21:50 mclareni Exp $
*
* $Log: streamw.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:50  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_APOLLO)
      SUBROUTINE WRITE_$STREAM(BUFFER,RET,STATUS_$RETURNED)
%INCLUDE '/sys/ins/base.ins.ftn'
%INCLUDE '/sys/ins/error.ins.ftn'
%INCLUDE '/sys/ins/streams.ins.ftn'
      INTEGER*4 RET,STATUS_$RETURNED,BUFFER(1),KEY(3)
      INTEGER*2 STREAM_$ID
#include "epio/epiocom.inc"
C
C     HERE WE ACTUALLY DO THE WRITE
C
C--- GET THE STREAM_$ID
      STREAM_$ID = INT2(LIST(ISTART+25))
C--- GO AND WRITE
      CALL STREAM_$PUT_REC(
     1 STREAM_$ID,
     2  IADDR(BUFFER),
     3   RET,
     6    KEY,
     7     STATUS_$RETURNED)
      IF(STATUS_$RETURNED.NE.STATUS_$OK)
     +CALL ERROR_$PRINT(STATUS_$RETURNED)
      END
#endif
