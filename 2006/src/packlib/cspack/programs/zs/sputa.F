*
* $Id: sputa.F,v 1.2 1996/04/11 14:54:14 cernlib Exp $
*
* $Log: sputa.F,v $
* Revision 1.2  1996/04/11 14:54:14  cernlib
* Zserv/pawserv used to be build from two patches; these were put both into this
* directory.
* The #includes in all files copied from the other directory had to updated.
*
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SPUTA(CHF)
*
*          Transfer a text file from client
*
#include "hbook/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
#include "cspack/czunit.inc"
      CHARACTER*(*) CHF
      CHARACTER*4   CHOPT
      CHARACTER*4   OPT
      CHARACTER*80 LOCAL
      DIMENSION NCC(1024)
      CHARACTER*3077 CHLEN
      INTEGER SRECVSTR
*
*_______________________________________
*
      IBL   = INDEX(CHF,' ')
      LCHF  = LENOCC(CHF)
      CHOPT = ' '
      IF(IBL.NE.0) THEN
#if defined(CERNLIB_IBMVM)
         LOCAL = '/'//CHF(1:IBL-1)
#endif
#if !defined(CERNLIB_IBMVM)
         LOCAL = CHF(1:IBL-1)
#endif
         IF(LCHF.GT.IBL) CHOPT = CHF(IBL+1:LCHF)
      ELSE
#if defined(CERNLIB_IBMVM)
         LOCAL = '/'//CHF
#endif
#if !defined(CERNLIB_IBMVM)
         LOCAL = CHF
#endif
      ENDIF
*
*           Create local file
*
      CHMAIL='OK'
#if defined(CERNLIB_UNIX)
      IF(INDEX(CHOPT,'C').EQ.0) CALL CUTOL(LOCAL)
#endif
#if defined(CERNLIB_UNIX)
      OPEN(UNIT=LUNXZO,FILE=LOCAL,STATUS='UNKNOWN',IOSTAT=ISTAT)
#endif
#if defined(CERNLIB_VAXVMS)
      OPEN(UNIT=LUNXZO,FILE=LOCAL,STATUS='NEW',IOSTAT=ISTAT,
     +     CARRIAGECONTROL='LIST')
#endif
#if defined(CERNLIB_IBMMVS)
      LRECL = 80
      OPT   = 'OF'
      IF(INDEX(CHOPT,'V').NE.0) OPT = 'OFV'
      CALL SZOPEN(LUNXZO,LOCAL,LRECL,OPT,ISTAT)
#endif
#if defined(CERNLIB_IBMVM)
      NCH=LENOCC(LOCAL)
      DO 5 I=2,NCH
         IF(LOCAL(I:I).EQ.'.')LOCAL(I:I)=' '
   5  CONTINUE
         IF(INDEX(CHOPT,'V').NE.0) THEN
         CALL FILEINF(IRC,'RECFM','U')
         ENDIF
      OPEN(UNIT=LUNXZO,FILE=LOCAL,STATUS='UNKNOWN',IOSTAT=ISTAT,
     +     FORM='FORMATTED')
#endif
      IF(ISTAT.NE.0)GO TO 95
*
*           Inform client if file opened on local node
*
  50  CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
      IF(CHMAIL.EQ.'KO')GO TO 99
*
*           Transfer data
*
      NR=0
20    CONTINUE
      NMAX=3077
#if defined(CERNLIB_DECNET)
*
*     DECnet...
*
      IF(IPROT.EQ.1) THEN
      CALL CZDGTS(CHLEN,NMAX,ISTAT)
      IF(ISTAT.NE.0) GOTO 99
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
11    CONTINUE
#endif
      IF(NTOT.LE.0)GO TO 40
*
      CALL CZGETC(NTOT,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
      NR=NR+NTOT
      I1=1
      DO 30 I=1,1024
         IF(NCC(I).LT.0)THEN
            IF(NCC(I).EQ.-2)GO TO 40
            GO TO 20
         ENDIF
         IF(NCC(I).EQ.0)THEN
            WRITE(LUNXZO,'(A)',ERR=99) ' '
         ELSE
            I2=I1+NCC(I)-1
            WRITE(LUNXZO,'(A)',ERR=99) CHBUF(I1:I2)
            I1=I2+1
         ENDIF
  30  CONTINUE
      GO TO 20
*
  40  CONTINUE
      CLOSE(LUNXZO)
      GO TO 99
*
  95  CHMAIL='KO'
      GO TO 50
*
  99  END
 
