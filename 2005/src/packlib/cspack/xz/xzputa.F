*
* $Id: xzputa.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzputa.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZPUTA(LOCAL,REMOTE,CHOPT,IRC)
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
      INTEGER        SSENDSTR
      CHARACTER*(*)  REMOTE,LOCAL
      CHARACTER*8    CHOPTT
      CHARACTER*255  CHLINE
      CHARACTER*255  CHFILE
      CHARACTER*8    DELTIM
      CHARACTER*3077 CHLEN
      DIMENSION      NCC(1024)
#include "cspack/czoptd.inc"
      DATA NCC/1024*-1/
      DATA NENTRY/0/
#include "cspack/czoptu.inc"
#include "cspack/czopen.inc"
#if defined(CERNLIB_IBM)
*
*     Allow file transfer of RECFM V files
*
      IF(NENTRY.EQ.0) THEN
         CALL ERRSET(212,256,-1,1,1)
         NENTRY = 1
      ENDIF
#endif
 
      IRC  = 0
*     IMAX = 16240
*     IF(IPROT.EQ.1) IMAX = 432
      IMAX = 16320
      IF(IPROT.EQ.1) IMAX = 512
*
*        Open local text file
*
      IOPTV=0
      CHFILE=LOCAL
      ISTAT = 0
      IF(IOPTA.EQ.0) THEN
#if defined(CERNLIB_UNIX)
      IF(IOPTC.EQ.0) CALL CUTOL(CHFILE)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_OS9))
      OPEN(UNIT=LUNXZI,FILE=CHFILE,FORM='FORMATTED',STATUS='OLD',
     +     IOSTAT=ISTAT)
      IF(ISTAT.NE.0) GOTO 95
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_APOLLO)||defined(CERNLIB_OS9))
      OPEN(UNIT=LUNXZI,FILE=CHFILE,STATUS='READONLY',IOSTAT=ISTAT)
      IF(ISTAT.NE.0) GOTO 95
#endif
#if defined(CERNLIB_VAXVMS)
      OPEN(UNIT=LUNXZI,FILE=CHFILE,STATUS='OLD',READONLY,IOSTAT=ISTAT)
      IF(ISTAT.NE.0) GOTO 95
#endif
#if defined(CERNLIB_IBMMVS)
      CALL KUOPEN(LUNXZI,CHFILE(1:LENOCC(CHFILE)),'OLD',ISTAT)
      IF(ISTAT.NE.0) GOTO 95
#endif
#if defined(CERNLIB_IBMVM)
      CHFILE='STATE '//LOCAL
      DO 2 I=7,64
         IF(CHFILE(I:I).EQ.'.')CHFILE(I:I)=' '
   2  CONTINUE
      CALL VMCMS(CHFILE,ISTAT)
      IF(ISTAT.NE.0) GOTO 95
C
      CHFILE='/'//LOCAL
      NCH=LENOCC(CHFILE)
      DO 5 I=1,NCH
         IF(CHFILE(I:I).EQ.'.')CHFILE(I:I)=' '
   5  CONTINUE
      OPEN(UNIT=LUNXZI,FILE=CHFILE,STATUS='OLD',IOSTAT=ISTAT,
     +     FORM='UNFORMATTED')
 
      IF(ISTAT.NE.0) GOTO 95
#endif
      ENDIF
*
*        Send message to remote machine with the file parameters
*
      NCHR=LENOCC(REMOTE)
      NCHO=LENOCC(CHOPT)
      CHOPTT = CHOPT
      IF(NCHO.EQ.0) THEN
         CHOPTT = ' '
         NCHO   = 1
      ENDIF
 
      CHMAIL='PUTA :'//REMOTE(1:NCHR)//' '//CHOPTT(1:NCHO)
      CALL CZPUTA(CHMAIL,IRC)
      IF(IRC.NE.0)GOTO 99
*
*          Verify that text file has been opened by server
*
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GOTO 90
      IF(CHMAIL(1:2).NE.'OK')GOTO 90
*
*          Now transfer the file
*
      NR = 0
      IF(IOPTS.NE.0) THEN
         CALL TIMED(T)
         CALL CZRTIM(DELTIM)
      ENDIF
 
      IEND   = 0
  10  NLINES = 1
      NTOT   = 0
      I1     = 1
      ICONT  = 0
  20  CONTINUE
#if defined(CERNLIB_IBMVM)
      READ(LUNXZI,NUM=NCH,END=50,ERR=50)CHLINE
#endif
#if !defined(CERNLIB_IBMVM)
      READ(LUNXZI,'(A)',END=50,ERR=50)CHLINE
      NCH=LENOCC(CHLINE)
#endif
      NR = NR + NCH
*
*     Do we have room for this record in the current buffer?
*
      IF(I1+NCH-1.GT.IMAX) THEN
         ICONT = 1
         IF(NLINES.LE.1024)NCC(NLINES)=-1
         GOTO 30
      ENDIF
 
      IF(NCH.EQ.0)THEN
         NCC(NLINES)=0
      ELSE
         NCC(NLINES)=NCH
         I2=I1+NCH-1
         CHBUF(I1:I2)=CHLINE(1:NCH)
         I1=I2+1
         NTOT=NTOT+NCH
      ENDIF
      IF(I1+NCH-1.GT.IMAX)THEN
         IF(NLINES.LT.1024)NCC(NLINES+1)=-1
         GOTO 30
      ENDIF
      NLINES=NLINES+1
      IF(NLINES.LE.1024)GOTO 20
*
  30  CONTINUE
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_CONVEX))
      WRITE(CHLEN,1000)NTOT,NCC
 1000 FORMAT(I5,4I3,255I3,255I3,255I3,255I3)
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_CONVEX)
      WRITE(CHLEN(1:5),1000)NTOT
 1000 FORMAT(I5)
      IOFF = 6
      JOFF = 1
      DO 11 II=1,16
      WRITE(CHLEN(IOFF:IOFF+95),'(32I3)') (NCC(JJ),JJ=JOFF,JOFF+31)
      IOFF = IOFF + 96
      JOFF = JOFF + 32
11    CONTINUE
#endif
      NTOTAL = 3077
#if defined(CERNLIB_DECNET)
*
*     DECnet...
*
      IF(IPROT.EQ.1) THEN
         CALL CZDPTS(CHLEN,NTOTAL,ISTAT)
         IF(ISTAT.NE.0) GOTO 97
      ELSE
#endif
#if !defined(CERNLIB_IBM)||defined(CERNLIB_TCPSOCK)
*
*     TCP/IP (with socket library)
*
      NBYTES=SSENDSTR(ISKOUT,CHLEN,NTOTAL)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_TCPSOCK))
*
*     TCP/IP on IBM without socket library->PASCAL version of TCPAW
*
      CALL SSEND(ISKOUT,CHLEN,NTOTAL,NBYTES)
      IF(NBYTES.LT.NTOTAL) GOTO 97
#endif
#if defined(CERNLIB_DECNET)
      ENDIF
#endif
 
      CALL CZPUTC(NTOT,ISTAT)
      IF(ISTAT.NE.0)GOTO 97
*
*     Still a record in the current buffer?
*
      IF(ICONT.NE.0) THEN
         ICONT  = 0
         NCC(1) = NCH
         CHBUF(1:NCH) = CHLINE(1:NCH)
         I1     = NCH + 1
         NTOT   = NCH
         NLINES = 2
         GOTO 20
      ENDIF
 
      IF(IEND.EQ.0)GOTO 10
*
      CLOSE(LUNXZI)
      IF(IDEBXZ.GE.0) PRINT 2000
 2000 FORMAT(' File transfer completed')
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
      GOTO 99
*
  50  IEND=1
      NCC(NLINES)=-2
      GOTO 30
*
  90  WRITE(IXPRNT,*) 'Cannot open remote file'
      IRC = 1
      CLOSE(LUNXZI)
      GOTO 99
*
  95  WRITE(IXPRNT,*) 'Cannot open local file'
      IRC = 2
      GOTO 99
*
  97  WRITE(IXPRNT,*) 'Problems in transferring file'
      IRC = 3
      CLOSE(LUNXZI)
*
  99  END
