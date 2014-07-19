*
* $Id: sgeta.F,v 1.2 1996/04/11 14:54:07 cernlib Exp $
*
* $Log: sgeta.F,v $
* Revision 1.2  1996/04/11 14:54:07  cernlib
* Zserv/pawserv used to be build from two patches; these were put both into this
* directory.
* The #includes in all files copied from the other directory had to updated.
*
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SGETA(CHF)
*
*          Transfer a text file to client
*
#include "hbook/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
#include "cspack/czunit.inc"
      CHARACTER*(*)  CHF
      CHARACTER*8    CHOPT
      CHARACTER*255  CHLINE
      CHARACTER*80   CHFILE
      CHARACTER*80   REMOTE,LOCAL
      CHARACTER*3077 CHLEN
*     CHARACTER*2053 CHLEN
      DIMENSION      NCC(1024)
      INTEGER        SSENDSTR
      DATA NCC/1024*-1/
*
*
*        Open file on server
*
      IOPTV=0
      LRECL=255
      IMAX = 16320
      IF(IPROT.EQ.1) IMAX = 512
      LCHF  = LENOCC(CHF)
      LBLNK = INDEX(CHF(1:LCHF),' ')
      IF(LBLNK.NE.0) THEN
         CHFILE = CHF(1:LBLNK-1)
         CHOPT  = CHF(LBLNK+1:LCHF)
         LFILE  = LBLNK-1
      ELSE
         CHFILE = CHF(1:LCHF)
         CHOPT  = ' '
         LFILE  = LCHF
      ENDIF
#if defined(CERNLIB_UNIX)
      IF(INDEX(CHOPT,'C').EQ.0) CALL CUTOL(CHFILE)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_OS9))
      OPEN(UNIT=LUNXZI,FILE=CHFILE,STATUS='OLD',IOSTAT=ISTAT)
#endif
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_OS9)
      OPEN(UNIT=LUNXZI,FILE=CHFILE,STATUS='READONLY',
     +         IOSTAT=ISTAT)
#endif
#if defined(CERNLIB_VAXVMS)
      OPEN(UNIT=LUNXZI,FILE=CHFILE,STATUS='OLD',READONLY,IOSTAT=ISTAT)
#endif
#if defined(CERNLIB_IBMMVS)
      CALL SZOPEN(LUNXZI,CHFILE,LRECL,'IF',ISTAT)
#endif
#if defined(CERNLIB_IBMVM)
      CHFILE='/'//CHF(1:LFILE)
      NCH=LENOCC(CHFILE)
      DO 5 I=1,NCH
         IF(CHFILE(I:I).EQ.'.')CHFILE(I:I)=' '
   5  CONTINUE
      CALL FILEINF(IC,'RECFM','U')
      OPEN(UNIT=LUNXZI,FILE=CHFILE,STATUS='OLD',IOSTAT=ISTAT,
     +     FORM='UNFORMATTED')
 
#endif
*
*        Inform client if file is opened
*
      IF(ISTAT.NE.0)LRECL=0
      CHMAIL=' '
      WRITE(CHMAIL,'(I10)')LRECL
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GOTO 99
      IF(LRECL.EQ.0)GOTO 99
*
*        Check if client has opened his file
*
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GOTO 99
      IF(CHMAIL(1:2).NE.'OK')GOTO 99
*
*        Start transfer
*
      IEND   = 0
  10  NLINES = 1
      NTOT   = 0
      I1     = 1
      ICONT  = 0
  20  CONTINUE
#if defined(CERNLIB_IBMVM)
      READ(LUNXZI,NUM=NCH,END=50)CHLINE
#endif
#if !defined(CERNLIB_IBMVM)
      READ(LUNXZI,'(A)',END=50,ERR=50)CHLINE
      NCH=LENOCC(CHLINE)
#endif
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
         CHBUF(I1:I2)=CHLINE
         I1=I2+1
         NTOT=NTOT+NCH
      ENDIF
      IF(I1+NCH-1.GT.IMAX)THEN
         IF(NLINES.LT.1024)NCC(NLINES+1)=-1
         GOTO 30
      ENDIF
      NLINES=NLINES+1
      IF(NLINES.LE.1024) GOTO 20
*
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_CONVEX))
  30  WRITE(CHLEN,1000)NTOT,NCC
 1000 FORMAT(I5,4I3,255I3,255I3,255I3,255I3)
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_CONVEX)
  30  WRITE(CHLEN(1:5),1000)NTOT
 1000 FORMAT(I5)
      IOFF = 6
      JOFF = 1
      DO 11 II=1,8
      WRITE(CHLEN(IOFF:IOFF+95),'(32I3)') (NCC(JJ),JJ=JOFF,JOFF+31)
      IOFF = IOFF + 96
      JOFF = JOFF + 32
11    CONTINUE
#endif
      NTOTAL = 3077
*     NTOTAL = 2053
#if defined(CERNLIB_DECNET)
*
*     DECnet...
*
      IF(IPROT.EQ.1) THEN
      CALL CZDPTS(CHLEN,NTOTAL,ISTAT)
      IF(ISTAT.NE.0) GOTO 99
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
      IF(NBYTES.LT.NTOTAL) GOTO 99
#endif
#if defined(CERNLIB_DECNET)
      ENDIF
#endif
 
      CALL CZPUTC(NTOT,ISTAT)
*     IF(ISTAT.NE.0)GOTO 99
*
*     Still a record in the current buffer?
*
      IF(ICONT.NE.0) THEN
         ICONT  = 0
         NCC(1) = NCH
         CHBUF(1:NCH) = CHLINE
         I1     = NCH + 1
         NTOT   = NCH
         NLINES = 2
         GOTO 20
      ENDIF
 
      IF(IEND.EQ.0)GOTO 10
*
      CLOSE(LUNXZI)
      GOTO 99
*
  50  IEND=1
      NCC(NLINES)=-2
      GOTO 30
*
  99  END
