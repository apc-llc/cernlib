*
* $Id: cztcp.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: cztcp.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZTCP(IBUF,ICONTR)
#include "cspack/czsock.inc"
      COMMON/QUEST/IQUEST(100)
      CHARACTER*80 CHMAIL
      INTEGER SRECV,SSEND
      INTEGER SRECVSTR,SSENDSTR
      DIMENSION IBUF(1),IHAND(4)
      DIMENSION ICONTR(2)
      CHARACTER*4 HAND
*
#if defined(CERNLIB_VAXVMS)
      IF(IPROT.NE.0) THEN
         CALL CZDNET(IBUF,ICONTR)
         RETURN
      ENDIF
#endif
      ICASE=ICONTR(1)+1
      IF(ICASE.LT.3)THEN
         NBYTES=4*LBUF
      ELSE
         NBYTES=ICONTR(2)*4
      ENDIF
*     PRINT *,'CZTCP enter for ICASE/NBYTES = ',ICASE,NBYTES
      GO TO (100,200,300,400),ICASE
      IF(ICASE.LE.0.OR.ICASE.GT.4)THEN
         IF(LUNCZ.NE.0)WRITE(LUNCZ,*)' Error in ICASE'
         RETURN
      ENDIF
*
*           Receive Binary
*
 100  NTOT=NBYTES
      ISTAT=0
      NMAX=NTOT
#if !defined(CERNLIB_IBM)||defined(CERNLIB_TCPSOCK)
      NBYTES=SRECV(ISKIN,IBUF,NMAX)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_TCPSOCK))
      CALL BRECV(ISKIN,IBUF,NMAX,NBYTES)
#endif
#if defined(CERNLIB_TCPSOCK)
*     IF(NBYTES.EQ.0) NBYTES = NMAX
*     nbytes = nmax
#endif
      IF(NBYTES.NE.NMAX)THEN
         IF(LUNCZ.NE.0)WRITE(LUNCZ,*)'Problem in receiving Binary'
         ISTAT=1
      ELSE
         ISTAT=0
      ENDIF
      IQUEST(1)=ISTAT
#if defined(CERNLIB_DEBUG)
      IF(LUNCZ.NE.0)WRITE(LUNCZ,1101)NBYTES
 1101 FORMAT(' CZTCP receiving',I10,' bytes')
#endif
      GO TO 999
*
*           Send Binary
*
 200  NSEND=0
      IREC=1
#if defined(CERNLIB_DEBUG)
      IF(LUNCZ.NE.0)WRITE(LUNCZ,1102)NBYTES
 1102 FORMAT(' CZTCP sending',I10,' bytes')
      IF(LUNCZ.NE.0)WRITE(LUNCZ,'(10Z9)') (IBUF(I),I=1,10)
#endif
#if !defined(CERNLIB_IBM)||defined(CERNLIB_TCPSOCK)
 210  NSEND=SSEND(ISKOUT,IBUF(IREC),NBYTES)
#endif
#if defined(CERNLIB_TCPSOCK)
*     IF((NSEND.EQ.0).OR.(NSEND.GT.NBYTES)) NSEND = NBYTES
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_TCPSOCK))
*
*     Fix for crummy IBM TCP/IP
*
      IF(NBYTES.LE.8192) THEN
 210  CALL BSEND(ISKOUT,IBUF(IREC),NBYTES,NSEND)
      ELSE
      NLOOP = NBYTES/8192
      NREST = MOD(NBYTES,8192)
      DO 211 JLOOP=1,NLOOP
      CALL BSEND(ISKOUT,IBUF(IREC),8192,NSEND)
      IREC  = IREC + 8192
211   CONTINUE
      CALL BSEND(ISKOUT,IBUF(IREC),NREST,NSEND)
      NSEND = NBYTES
      ENDIF
#endif
      IF(NSEND.NE.NBYTES)THEN
         ISTAT=NSEND-NBYTES
         IF(LUNCZ.NE.0)WRITE(LUNCZ,*)
     +      'Problem in sending binary, NSEND/NBYTES = ',
     +                  NSEND,NBYTES
      ELSE
         ISTAT=0
      ENDIF
      IQUEST(1)=ISTAT
      GO TO 999
*
*           Receive Character
*
 300  CONTINUE
      NMAX=80
      HAND='CZOK'
#if (defined(CERNLIB_OS9))&&(!defined(CERNLIB_IBM))
      NBYTES=SRECVSTR(ISKIN,IBUF,NMAX)
      IF(NBYTES.NE.NMAX)GO TO 350
      NK=SSENDSTR(ISKOUT,IHAND,4)
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_OS9))
      NBYTES=SRECVSTR(ISKIN,CHMAIL,NMAX)
      IF(NBYTES.NE.NMAX)GO TO 350
      NK=SSENDSTR(ISKOUT,HAND,4)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_TCPSOCK))
      NBYTES=SRECVSTR(ISKIN,CHMAIL,NMAX)
      IF(NBYTES.NE.NMAX)GO TO 350
      NK=SSENDSTR(ISKOUT,HAND,4)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_TCPSOCK))
      CALL SRECV(ISKIN,CHMAIL,NMAX,NBYTES)
      IF(NBYTES.NE.NMAX)GO TO 350
      CALL SSEND(ISKOUT,HAND,4,NK)
#endif
      ICONTR(1)=0
#if !defined(CERNLIB_OS9)
      CALL UCTOH(CHMAIL,IBUF,4,80)
#endif
#if defined(CERNLIB_DEBUG)
      IF(LUNCZ.NE.0)WRITE(LUNCZ,1103)NBYTES,(IBUF(I),I=1,20)
 1103 FORMAT(' CZTCP receiving',I4,' characters',/,1X,20A4)
#endif
      GO TO 999
 350  ICONTR(1)=1
      IF(LUNCZ.NE.0)WRITE(LUNCZ,*)'Problem in receiving character'
      GO TO 999
*
*           Send Character
*
 400  CHMAIL=' '
#if (defined(CERNLIB_OS9))&&(!defined(CERNLIB_IBM))
      NSEND=SSENDSTR(ISKOUT,IBUF,NBYTES)
      IF(NSEND.NE.NBYTES)GO TO 450
      NK=SRECVSTR(ISKIN,IHAND,4)
#endif
#if (!defined(CERNLIB_OS9))&&(!defined(CERNLIB_IBM))
      CALL UHTOC(IBUF,4,CHMAIL,NBYTES)
      NSEND=SSENDSTR(ISKOUT,CHMAIL,NBYTES)
      IF(NSEND.NE.NBYTES)GO TO 450
      NK=SRECVSTR(ISKIN,HAND,4)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_TCPSOCK))
      CALL UHTOC(IBUF,4,CHMAIL,NBYTES)
      NSEND=SSENDSTR(ISKOUT,CHMAIL,NBYTES)
      IF(NSEND.NE.NBYTES)GO TO 450
      NK=SRECVSTR(ISKIN,HAND,4)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_TCPSOCK))
      CALL UHTOC(IBUF,4,CHMAIL,NBYTES)
      CALL SSEND(ISKOUT,CHMAIL,NBYTES,NSEND)
      IF(NSEND.NE.NBYTES)GO TO 450
      CALL SRECV(ISKIN,HAND,4,NK)
#endif
      ICONTR(1)=0
#if defined(CERNLIB_DEBUG)
      IF(LUNCZ.NE.0)WRITE(LUNCZ,1104)NSEND,(IBUF(I),I=1,20)
 1104 FORMAT(' CZTCP sending',I4,' characters',/,1X,20A4)
#endif
      GO TO 999
 450  ICONTR(1)=1
      IF(LUNCZ.NE.0)WRITE(LUNCZ,*)
     +   'Problem in sending character, NSEND/NBYTES = ',
     +              NSEND,NBYTES
*
 999  END
 
