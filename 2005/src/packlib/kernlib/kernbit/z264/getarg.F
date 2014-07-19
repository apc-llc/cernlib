*
* $Id: getarg.F,v 1.1.1.1 1996/02/15 17:47:52 mclareni Exp $
*
* $Log: getarg.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:52  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE GETARG(JW,PARM)
C
C CERN PROGLIB#    Z264 IARGC           .VERSION KERNVAX  x.xx  911130
C
C Returns JWth argument in command line
C Author: Miguel Marquina 91/11/30
C
C Mods          Date   Comments
c V.Fine      94/01/18 Windows/NT support
c J.Shiers    93/11/15 Handle getarg(0,parm) case
*                      ISTAT returned in /SLATE/
c M.Marquina  92/12/05 Treat quote-delimited strings as single args
C
      COMMON/Z264/IREAD,ISLEN,STRING
      COMMON/SLATE/IS(40)
      CHARACTER*255 STRING
      LOGICAL LB,LQ
      CHARACTER*(*) PARM
#if defined(CERNLIB_VAXVMS)
      INCLUDE '($JPIDEF)'
*
*     Imagename
*
      IF(JW.EQ.0) THEN
         IS(1) = 0
         ISTAT = LIB$GETJPI(JPI$_IMAGNAME,,,,STRING,%REF(LENSTR))
         IF(.NOT.ISTAT) THEN
            IS(1) = ISTAT
            PARM  = ' '
            GOTO 99
         ENDIF
         ISTAT = LIB$TRIM_FILESPEC(STRING(1:LENSTR),PARM,LEN(PARM),)
      ELSE

         IF(IREAD.EQ.0) THEN
            STRING=' '
            IFL=0
            IRC=LIB$GET_FOREIGN(STRING,,ISLEN,IFL)
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
      ENDIF
#endif
#if defined(CERNLIB_WINNT)
      IF(IREAD.EQ.0) THEN
         STRING=' '
         IFL=0
         CALL GETCOM(STRING,ISLEN)
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
            IF(NW.EQ.JW.AND.IP1.EQ.0) IP1=N
            IF(LB) LB=.FALSE.
         ENDIF
         IF(LQ) GOTO 10
         IF(STRING(N:N).EQ.' '.AND..NOT.LB) THEN
            LB=.TRUE.
            NW=NW+1
         ENDIF
         IF(NW.EQ.JW+1) THEN
            IP2=N-1
            GOTO 20
         ENDIF
   10 CONTINUE
   20 CONTINUE
      PARM=' '
      IF(IP1.GT.0) THEN
         IF(STRING(IP1:IP1).EQ.'"') IP1=IP1+1
         IF(STRING(IP2:IP2).EQ.'"') IP2=IP2-1
         IF(IP1.LE.IP2) PARM=STRING(IP1:IP2)
      ENDIF
#endif
99    RETURN
      END
