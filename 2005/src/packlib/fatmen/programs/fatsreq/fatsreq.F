*
* $Id: fatsreq.F,v 1.1.1.1 1996/03/07 15:17:40 mclareni Exp $
*
* $Log: fatsreq.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:40  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATSREQ
*
*     Issue SYSREQ command with retry
*
      CHARACTER*255 CHLINE
#include "fatmen/tmsrep.inc"
#if defined(CERNLIB_UNIX)
      CHARACTER*255 CHARG
#endif
 
#if defined(CERNLIB_IBM)
      CALL GOPARM(LLINE,CHLINE)
#endif
#if defined(CERNLIB_VAXVMS)
      IFLAG = 0
      CALL LIB$GET_FOREIGN(CHLINE,,LLINE,IFLAG)
#endif
#if defined(CERNLIB_UNIX)
      NARGS = IARGC(DUMMY)
      LLINE = 0
 
      DO 10 I=1,NARGS
         CALL GETARG(I,CHARG)
         LARG = LENOCC(CHARG)
         IF(I.EQ.1) THEN
            CHLINE = CHARG(1:LARG)
            LLINE  = LARG
         ELSE
            CHLINE(LLINE+1:) = ' '//CHARG(1:LARG)
            LLINE  = LLINE + LARG + 1
         ENDIF
   10 CONTINUE
#endif
 
      IF(LLINE.EQ.0) THEN
         WRITE(6,9001)
 9001 FORMAT(' FATSREQ. service name missing')
#if defined(CERNLIB_VAXVMS)
         CALL SYS$EXIT(%VAL(3))
#endif
#if defined(CERNLIB_UNIX)
         CALL FAEXIT(3)
#endif
#if defined(CERNLIB_IBM)
         STOP 3
#endif
      ELSE
 
         L      = LENREP
         IBLANK = INDEX(CHLINE(1:LLINE),' ')
         IF(IBLANK.EQ.0) THEN
            WRITE(6,9002)
 9002 FORMAT(' FATSREQ. command missing')
#if defined(CERNLIB_VAXVMS)
            CALL SYS$EXIT(%VAL(3))
#endif
#if defined(CERNLIB_UNIX)
            CALL FAEXIT(3)
#endif
#if defined(CERNLIB_IBM)
            STOP 3
#endif
         ENDIF
         CALL FMSREQ(CHLINE(1:IBLANK-1),CHLINE(IBLANK+1:LLINE),
     +               IRC,TMSREP,L)
         IF(IRC.NE.0) THEN
            WRITE(6,9003) IRC
 9003 FORMAT(' FATSREQ. RC from FMSREQ = ',I6)
#if defined(CERNLIB_IBM)
            STOP 8
#endif
#if defined(CERNLIB_VAXVMS)
            CALL SYS$EXIT(%VAL(44))
#endif
#if defined(CERNLIB_UNIX)
            CALL FAEXIT(8)
#endif
         ENDIF
 
         DO 20 I=1,L
            WRITE(6,9004) TMSREP(I)(1:LENOCC(TMSREP(I)))
 9004 FORMAT(1X,A)
   20    CONTINUE
 
#if defined(CERNLIB_VAXVMS)
         CALL SYS$EXIT(%VAL(1))
#endif
#if !defined(CERNLIB_VAXVMS)
         STOP
#endif
 
      ENDIF
 
      END
