*
* $Id: getarg.F,v 1.1.1.1 1996/02/15 17:51:51 mclareni Exp $
*
* $Log: getarg.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:51  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE GETARG(JW,PARM)
C
C CERN PROGLIB#    Z264 IARGC           .VERSION KERNCMS  1.05  910308
C
C Returns JWth argument in command line
C Author: Miguel Marquina 91/03/08
C
C Mods          Date   Comments
C M.Marquina  91/09/01 Initialize STRING before calling GOPARM
C                      Set IP2=ISLEN for the last argument
C M.Marquina  92/12/05 Treat quote-delimited strings as single args
C
      COMMON/Z264/IREAD,ISLEN,STRING
      CHARACTER*100 STRING
      LOGICAL LB,LQ
      CHARACTER*(*) PARM
      IF(IREAD.EQ.0) THEN
        STRING=' '
        CALL GOPARM(ISLEN,STRING)
        IREAD=1
      ENDIF
      NW =0
      LB =.TRUE.
      LQ =.FALSE.
      IP1=0
      IP2=ISLEN
      DO 10 N=1,ISLEN
        IF(STRING(N:N).NE.' ') THEN
           IF(STRING(N:N).EQ.'"') LQ=.NOT.LQ
           IF(NW.EQ.JW-1.AND.IP1.EQ.0) IP1=N
           IF(LB) LB=.FALSE.
        ENDIF
        IF(LQ) GOTO 10
        IF(STRING(N:N).EQ.' '.AND..NOT.LB) THEN
          LB=.TRUE.
          NW=NW+1
        ENDIF
          IF(NW.EQ.JW) THEN
            IP2=N-1
            GOTO 20
          ENDIF
10    CONTINUE
20    CONTINUE
                   PARM=' '
      IF(IP1.GT.0) THEN
        IF(STRING(IP1:IP1).EQ.'"') IP1=IP1+1
        IF(STRING(IP2:IP2).EQ.'"') IP2=IP2-1
        IF(IP1.LE.IP2) PARM=STRING(IP1:IP2)
      ENDIF
      RETURN
      END
