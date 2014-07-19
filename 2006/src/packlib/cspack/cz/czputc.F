*
* $Id: czputc.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czputc.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZPUTC(NTOT,ISTAT)
*
*          To send NTOT characters from CBUF
*
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
      INTEGER SSENDSTR
*
      ISTAT = 0
      IF(NTOT.LE.0) RETURN
#if defined(CERNLIB_DEBUG)
      IF(LUNCZ.NE.0)WRITE(LUNCZ,9001)NTOT
 9001 FORMAT(' CZPUTC: sending',I10,' bytes')
 9002 FORMAT(A)
      IF(NTOT.LT.80)THEN
         IF(LUNCZ.NE.0)WRITE(LUNCZ,9002)CHBUF(1:NTOT)
      ELSE
         IF(LUNCZ.NE.0)WRITE(LUNCZ,9002)CHBUF(1:80)
         ND=MIN(NTOT,159)
         IF(LUNCZ.NE.0)WRITE(LUNCZ,9002)CHBUF(81:ND)
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
      IF(IPROT.NE.0) THEN
         CALL CZDPTC(NTOT,ISTAT)
         RETURN
      ENDIF
#endif
*
#if !defined(CERNLIB_IBM)
      NBYTES=SSENDSTR(ISKOUT,CHBUF,NTOT)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_TCPSOCK))
      NBYTES=SSENDSTR(ISKOUT,CHBUF,NTOT)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_TCPSOCK))
*
*     Fix for crummy IBM TCP/IP
*
      IF(NTOT.LE.8192) THEN
         CALL SSEND(ISKOUT,CHBUF,NTOT,NBYTES)
      ELSE
         NLOOP  = NTOT/8192
         NREST  = MOD(NTOT,8192)
         IREC   = 1
         NBYTES = 0
         NSEND  = 8192
 
         DO 10  JLOOP=1,NLOOP
            CALL SSEND(ISKOUT,CHBUF(IREC:IREC+NSEND-1),NSEND,NSENT)
            IF(NSENT.LT.NSEND) GOTO 20
            IREC = IREC + NSEND
            NBYTES = NBYTES + NSENT
   10    CONTINUE
 
         IF(NREST.GT.0) THEN
            CALL SSEND(ISKOUT,CHBUF(IREC:NBYTES),NREST,NSENT)
            IF(NSENT.LT.NSEND) GOTO 20
            NBYTES = NBYTES + NSENT
         ENDIF
 
      ENDIF
 
   20 CONTINUE
#endif
      IF(NBYTES.LT.NTOT)THEN
         IF(LUNCZ.NE.0) WRITE(LUNCZ,9003) NBYTES,NTOT
9003  FORMAT(' Problem in sending text - NBYTES/NTOT = ',2I6)
         ISTAT=NBYTES
      ELSE
         ISTAT=0
      ENDIF
*
      END
