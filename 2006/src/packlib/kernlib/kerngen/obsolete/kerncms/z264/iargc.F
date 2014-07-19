*
* $Id: iargc.F,v 1.1.1.1 1996/02/15 17:51:51 mclareni Exp $
*
* $Log: iargc.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:51  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION IARGC(DUMMY)
C
C CERN PROGLIB#    Z264 IARGC           .VERSION KERNCMS  1.05  910308
C
C Returns number of arguments in command line
C Author: Miguel Marquina 91/03/08
C
C Mods          Date   Comments
C M.Marquina  91/09/01 Initialize STRING before calling GOPARM
C                      Return 0 when ISLEN=0
C M.Marquina  92/12/05 Treat quote-delimited strings as single args
C
      COMMON/Z264/IREAD,ISLEN,STRING
      CHARACTER*100 STRING
      LOGICAL LB,LQ
      IF(IREAD.EQ.0) THEN
        STRING=' '
        CALL GOPARM(ISLEN,STRING)
        IREAD=1
      ENDIF
      NW=0
      IF(ISLEN.EQ.0) GOTO 20
          LB=.TRUE.
          LQ=.FALSE.
      DO 10 N=1,ISLEN
        IF(STRING(N:N).EQ.'"') LQ=.NOT.LQ
        IF(LQ) GOTO 10
        IF(STRING(N:N).NE.' '.AND.LB) LB=.FALSE.
        IF(STRING(N:N).EQ.' '.AND..NOT.LB) THEN
          LB=.TRUE.
          NW=NW+1
        ENDIF
10    CONTINUE
      IF(STRING(N-1:N-1).NE.' ') NW=NW+1
20    CONTINUE
      IARGC=NW
      RETURN
      END
