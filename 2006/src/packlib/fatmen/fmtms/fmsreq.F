*
* $Id: fmsreq.F,v 1.1.1.1 1996/03/07 15:18:14 mclareni Exp $
*
* $Log: fmsreq.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:14  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSREQ(CHSERV,CHCOMM,IRC,CHREP,L)
#include "fatmen/faust.inc"
#include "fatmen/fatnet.inc"
      CHARACTER*(*) CHSERV,CHCOMM,CHREP(L)
      CHARACTER*240 COMAND
      CHARACTER*20  LINE
      PARAMETER     (ISLEEP=60)
      PARAMETER     (MAXTRY=1000)
#include "fatmen/fatbug.inc"
      SAVE NENTRY
      DATA NENTRY/0/
 
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_SGI))
#include "fatmen/errno.inc"
#endif
#if defined(CERNLIB_CRAY)
#include "fatmen/errcray.inc"
#endif
#if defined(CERNLIB_SGI)
#include "fatmen/errsgi.inc"
#endif
 
      LCHSRV = LENOCC(CHSERV)
      LCHCMM = LENOCC(CHCOMM)
 
      IRC     = 0
      NENTRY  = NENTRY + 1
      NRETRY  = 0
      NFSREQ  = NFSREQ + 1
 
      IF(IDEBFA.GE.3) PRINT *,'FMSREQ. enter for service ',
     +   CHSERV(1:LCHSRV),' command ',CHCOMM(1:LCHCMM)
 
      COMAND  = CHCOMM(1:LCHCMM)
 
      LBUF    = L
   10 CONTINUE
#if defined(CERNLIB_TMS)
      LENBUF  = LBUF
      CALL SYSREQ(CHSERV(1:LCHSRV),COMAND,
     +            IRC,CHREP,LENBUF)
*
*     Service (temporarily) unavailable)
*
      IF(IRC.EQ.3) THEN
         CALL SLEEPF(ISLEEP)
         GOTO 10
      ENDIF
*
*     Volume in use (actually, volume not Free or Held)
*
      IF(IRC.EQ.260) THEN
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9001) IRC,ISLEEP
 9001 FORMAT(' FMSREQ. return code ',I5,
     +      ' from TMS. Waiting ',I5,' seconds before retry')
         CALL SLEEPF(ISLEEP)
         GOTO 10
      ENDIF
*
*     Network errors
*
      IFIND = IUFIND(IABS(IRC),IFATRC,1,NERRS)
      IF(IFIND.LE.NERRS) THEN
*
*     Found a return code for which we retry
*
         NSECS = IRETRY(IFIND)
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9001) IRC,NSECS
         CALL SLEEPF(NSECS)
         GOTO 10
      ENDIF
*
*     Continuation message: was the buffer big enough to
*     fit the continuation command?
*
      IF(IRC.EQ.2.AND.LENBUF.GT.0) L = LENBUF
 
      IF(IRC.NE.2) L = LENBUF
*
*     'Safe' error messages (continuation, volume unknown, unavailable)
*
      IF(IRC.EQ.2.OR.IRC.EQ.8.OR.IRC.EQ.100.OR.
     +  (IRC.GE.312.AND.IRC.LE.315)) GOTO 30
*
*     Bad configuration file
*
      IF(IRC.EQ.-1009) THEN
         IF(NENTRY.EQ.1.AND.IDEBFA.GE.-3) THEN
            WRITE(LPRTFA,9002)
 9002 FORMAT(///,' FMSREQ. *** alarm *** ',//,
     +       '   ##    #         ##    #####   #    #',/,
     +       '  #  #   #        #  #   #    #  ##  ##',/,
     +       ' #    #  #       #    #  #    #  # ## #',/,
     +       ' ######  #       ######  #####   #    #',/,
     +       ' #    #  #       #    #  #   #   #    #',/,
     +       ' #    #  ######  #    #  #    #  #    #',//,
     +       ' FMSREQ. *** alarm *** ',///)
            WRITE(LPRTFA,9003)
 9003 FORMAT(' FMSREQ. !!! SYSREQ is not correctly installed on',
     +       ' this machine.',/,
     +       ' FMSREQ. Check P=SYSREQ,D=GETCONFE in CSPACK')
#endif
#if (defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX))&&(defined(CERNLIB_TMS))
            WRITE(LPRTFA,9004)
 9004 FORMAT(' FMSREQ. the configuration file should be: ')
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_TMS))
            WRITE(LPRTFA,9005)
 9005 FORMAT(' --> SYS$MANAGER:SYSREQ.CONF')
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_TMS))
            WRITE(LPRTFA,9006)
 9006 FORMAT(' --> /etc/shift.conf  (if the SHIFT s/w is installed)')
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_TMS))&&(defined(CERNLIB_SHIFT))
            WRITE(LPRTFA,9007)
 9007 FORMAT(' (FATMEN has been installed with the SHIFT flag)')
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_TMS))
            WRITE(LPRTFA,9008)
 9008 FORMAT(' --> /etc/sysreq.conf (otherwise)')
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_TMS))&&(!defined(CERNLIB_SHIFT))
            WRITE(LPRTFA,9009)
 9009 FORMAT(' (FATMEN has been installed without the SHIFT flag)')
#endif
#if defined(CERNLIB_TMS)
            WRITE(LPRTFA,9002)
      ENDIF
         GOTO 30
      ENDIF
*
*     Other errors
*
      IF(IRC.NE.0) THEN
         IF(NRETRY.GT.MAXTRY) THEN
            IF(IDEBFA.GE.-3) WRITE(LPRTFA,9010) IRC,NRETRY
 9010 FORMAT(' FMSREQ. return code ',I5,' from TMS.',
     +       ' Giving up after ',I6,' retries')
            GOTO 30
         ENDIF
 
         IF(IDEBFA.GE.-3.AND.NRETRY.EQ.0) THEN
*
*     Display messages from TMS
*
            WRITE(LPRTFA,9011) IRC
 9011 FORMAT(' FMSREQ. return code ',I5,' from SYSREQ.')
            WRITE(LPRTFA,9012) CHCOMM(1:LCHCMM),CHSERV(1:LCHSRV)
 9012 FORMAT('            Command was ',A,' for service ',A)
 
            DO 20 ILOOP=1,LENBUF
               PRINT *,'FMSREQ. ',CHREP(ILOOP)(1:LENOCC(CHREP(ILOOP)))
   20       CONTINUE
         ENDIF
 
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9001) IRC,ISLEEP
         CALL SLEEPF(ISLEEP)
         NRETRY = NRETRY + 1
         GOTO 10
      ENDIF
 
#endif
#if !defined(CERNLIB_TMS)
      IRC = 999
#endif
 
   30 CONTINUE
 
      END
