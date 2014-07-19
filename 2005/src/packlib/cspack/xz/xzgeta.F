*
* $Id: xzgeta.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzgeta.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZGETA(LOCAL,REMOTE,CHOPT,IRC)
*
*     Transfer the text file LOCAL to the remote node as REMOTE
*     CHOPT:   V - remote file is created with V format (IBM)
*              S - statistics on the file transfer are printed
*              A - local file has already been opened
*
#include "cspack/czunit.inc"
#include "cspack/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
#include "cspack/quest.inc"
      CHARACTER*(*)  REMOTE,LOCAL
#if defined(CERNLIB_IBM)
      CHARACTER*80   CHFILE
#endif
      CHARACTER*8    CHOPTT
      CHARACTER*8    DELTIM
      CHARACTER*3077 CHLEN
*     CHARACTER*2053 CHLEN
      INTEGER SRECVSTR
      DIMENSION NCC(1024)
#include "cspack/czopts.inc"
*
#include "cspack/czopen.inc"
*
*        Send message to remote machine to check if file exists
*        and get file parameters
*
      IRC = 0
 
      NCHR=LENOCC(REMOTE)
      NCHO=LENOCC(CHOPT)
      CHOPTT = CHOPT
      IF(NCHO.EQ.0) THEN
         CHOPTT = ' '
         NCHO   = 1
      ENDIF
 
      CHMAIL='GETA :'//REMOTE(1:NCHR)//' '//CHOPTT(1:NCHO)
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 90
*
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 90
      READ(CHMAIL,'(I10)')LRECL
      IF(LRECL.EQ.0)GO TO 90
*
*          Create local file
*
      ISTAT = 0
      IF(IOPTA.EQ.0) THEN
#if defined(CERNLIB_UNIX)
         IF(IOPTC.EQ.0) CALL CUTOL(LOCAL)
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_APOSR9)||defined(CERNLIB_OS9))
         OPEN(UNIT=LUNXZO,FILE=LOCAL,STATUS='UNKNOWN',RECL=LRECL,
     +   IOSTAT=ISTAT)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOSR9))&&(!defined(CERNLIB_OS9))
         OPEN(UNIT=LUNXZO,FILE=LOCAL,STATUS='UNKNOWN',IOSTAT=ISTAT)
#endif
#if defined(CERNLIB_VAXVMS)
         OPEN(UNIT=LUNXZO,FILE=LOCAL,STATUS='NEW',IOSTAT=ISTAT,
     +   CARRIAGECONTROL='LIST')
#endif
#if defined(CERNLIB_IBMMVS)
         CALL KUOPEN(LUNXZO,LOCAL(1:LENOCC(LOCAL)),'UNKNOWN',ISTAT)
#endif
#if defined(CERNLIB_IBMVM)
         CHFILE='/'//LOCAL
         DO 2 I=7,64
            IF(CHFILE(I:I).EQ.'.')CHFILE(I:I)=' '
    2    CONTINUE
*
*     Create local file as V format if requested
*
         IF(IOPTV.NE.0) THEN
            CALL FILEINF(LUNXZO,'RECFM','U')
         ENDIF
 
         OPEN(UNIT=LUNXZO,FILE=CHFILE,STATUS='UNKNOWN',IOSTAT=ISTAT)
#endif
      ENDIF
*
*          Inform server that local file has been created
*
      IF(ISTAT.NE.0)THEN
         CHMAIL='KO'
      ELSE
         CHMAIL='OK'
      ENDIF
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) GO TO 95
      IF(CHMAIL(1:2).EQ.'KO') GOTO 95
*
*          Start transfer
*
      NR=0
      IF(IOPTS.NE.0) THEN
         CALL TIMED(T)
         CALL CZRTIM(DELTIM)
      ENDIF
 
   20 CONTINUE
      NMAX=3077
*     NMAX=2053
#if defined(CERNLIB_DECNET)
*
*     DECnet...
*
      IF(IPROT.EQ.1) THEN
         CALL CZDGTS(CHLEN,NMAX,ISTAT)
         IF(ISTAT.NE.0)    GOTO 99
      ELSE
#endif
#if !defined(CERNLIB_IBM)||defined(CERNLIB_TCPSOCK)
*
*     TCP/IP (with socket library)
*
         NBYTES=SRECVSTR(ISKIN,CHLEN,NMAX)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_TCPSOCK))
*
*     TCP/IP on IBM without socket library->PASCAL version of TCPAW
*
         CALL SRECV(ISKIN,CHLEN,NMAX,NBYTES)
         IF(NBYTES.LT.NMAX)GO TO 99
#endif
#if defined(CERNLIB_DECNET)
      ENDIF
#endif
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_CONVEX))
      READ(CHLEN,2000)NTOT,NCC
 2000 FORMAT(I5,4I3,255I3,255I3,255I3,255I3)
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_CONVEX)
      READ (CHLEN(1:5),2000)NTOT
 2000 FORMAT(I5)
      IOFF = 6
      JOFF = 1
      DO 11 II=1,8
         READ (CHLEN(IOFF:IOFF+95),'(32I3)') (NCC(JJ),JJ=JOFF,JOFF+31)
         IOFF = IOFF + 96
         JOFF = JOFF + 32
   11 CONTINUE
#endif
      IF(NTOT.LE.0)GO TO 40
*
      CALL CZGETC(NTOT,ISTAT)
      IF(ISTAT.NE.0)GO TO 97
      NR=NR+NTOT
      I1=1
      DO 30 I=1,1024
         IF(NCC(I).LT.0)THEN
            IF(NCC(I).EQ.-2)GO TO 40
            GO TO 20
         ENDIF
         IF(NCC(I).EQ.0)THEN
            WRITE(LUNXZO,'(A)',ERR=97)' '
         ELSE
            I2=I1+NCC(I)-1
            WRITE(LUNXZO,'(A)',ERR=97)CHBUF(I1:I2)
            I1=I2+1
         ENDIF
   30 CONTINUE
      GO TO 20
*
   40 CONTINUE
      CLOSE(LUNXZO)
      IF(IDEBXZ.GE.0) WRITE(IXPRNT,*) ' File transfer completed'
      NKILO = NR / 1024
 
      IF(IOPTS.NE.0)THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
         READ(DELTIM,'(I2,1X,I2,1X,I2)') IHOUR,IMIN,ISEC
         NSECS = ISEC + IMIN*60 + IHOUR*3600
         IF(NSECS.LE.0) NSECS = 1
         RATE  = FLOAT(NKILO)/FLOAT(NSECS)
#include "cspack/xzstat.inc"
         WRITE(IXPRNT,*) ' Transferred ',NR,' bytes, transfer rate = ',
     +                   RATE,' KB/S'
         WRITE(IXPRNT,*) ' Elapsed time = ',DELTIM,' CP time = ',T,
     +                   ' sec.'
      ENDIF
      GO TO 99
*
*          Error
*
   90 WRITE(IXPRNT,*)  'Cannot open remote file'
      IRC = 1
      GO TO 99
   95 WRITE(IXPRNT,*)  'Cannot open local file'
      IRC = 2
      GO TO 99
   97 WRITE(IXPRNT,*)  'Problem in transferring file'
      IRC = 3
      CLOSE(LUNXZO)
*
   99 END
