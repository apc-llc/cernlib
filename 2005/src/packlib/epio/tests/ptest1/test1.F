*
* $Id: test1.F,v 1.1.1.1 1996/03/08 15:21:44 mclareni Exp $
*
* $Log: test1.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:44  mclareni
* Epio
*
*
#include "pilot.h"
      SUBROUTINE TEST1
C--- WRITE A WRONG BLOCK, READ AND CHECK ERRORS
#include "testc.inc"
#if defined(CERNLIB_UNIVAC)
      DIMENSION NACC(2)
      DATA IUN11/'11 'F/,IEOF/'EOFM'/,IEP/'EP  '/
#endif
#if defined(CERNLIB_APOLLO)
%INCLUDE '/sys/ins/base.ins.ftn'
%INCLUDE '/sys/ins/error.ins.ftn'
%INCLUDE '/sys/ins/streams.ins.ftn'
      INTEGER*4 STATUS_$RETURNED
      INTEGER*2 ATT_$REC(39)
      CHARACTER*10 FILNAM
      DATA FILNAM/'FOR011.DAT'/
      DATA (ATT_$REC(J),J=35,39)/'FO','R0','11','.D','AT'/
      DATA ATT_$REC(2)/10/
#endif
      NPW=12345
      N16=360
      CALL VFILL(IDAT,N16,NPW)
      CALL BUN16W(IDAT,1,IPAC,1,N16)
C--- NO. OF BITS
      CALL EPGETC(N,IDAT)
      NBIT=IDAT(4)
      NW=(N16*16)/NBIT
#if defined(CERNLIB_CDC)||defined(CERNLIB_CRAY)
      BUFFER OUT (11,1) (IPAC(1),IPAC(NW))
      IF(UNIT(11).GT.0.)  STOP 10
      REWIND 11
#endif
#if defined(CERNLIB_IBM)||defined(CERNLIB_NORD)
      CALL IORITE(11,IPAC,4*NW,IRET)
      IF(IRET.NE.0)  CALL ERREX(16)
      CALL IOMARK(11,IRET)
      IF(IRET.NE.0)  CALL ERREX(99)
      CALL IORWND(11,IRET)
      IF(IRET.NE.0)  CALL ERREX(17)
#endif
#if defined(CERNLIB_UNIVAC)
      IOD=IODEV('11  ')
      IF(IOD.GE.16) THEN
        BITS(IEP,19,18)=NW
        NACC(1)=2**18+LOC(IEP)
        NACC(2)=NW*2**18+LOC(IPAC)
        CALL FIOGW(IUN11,NACC,2,0,ISTAT)
        IAD=(NW+28)/28
        CALL FIOW(IUN11,IEOF,1,IAD,ISTAT)
       ELSE
        CALL FIOW(IUN11,IPAC,NW,0,ISTAT)
        CALL TIOWEF(IUN11,ISTAT)
        CALL TIOREW(IUN11,ISTAT)
       ENDIF
#endif
#if defined(CERNLIB_VAX)
        OPEN(11,NAME='FOR011',TYPE='NEW',FORM='UNFORMATTED',
     1 RECORDTYPE='FIXED',RECL=900,BLOCKSIZE=3600)
      WRITE(11) (IPAC(I),I=1,NW)
      CLOSE(11)
#endif
#if defined(CERNLIB_APOLLO)
      OPEN(
     1 UNIT=11,
     2  FILE='FOR011.DAT',
     3   FORM='UNFORMATTED',
#endif
#if (defined(CERNLIB_APOLLO))&&(defined(CERNLIB_APOMAG))
     4    STATUS='WRITE')
#endif
#if (defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_APOMAG))
     4    STATUS='NEW',
     5     RECL=900)
#endif
#if defined(CERNLIB_APOLLO)
      WRITE(11) (IPAC(I),I=1,NW)
      CLOSE(11)
#endif
#if defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX)
C  For Unix use Fortran open in EPOPEN to avoid portability problems
C     and set same blocksize for epread.
      CALL EPSETW(11,1,N16,IERR)
      CALL EPOPEN(1,IERR)
      WRITE(11) (IPAC(I),I=1,NW)
      CLOSE(11)
#endif
#if defined(CERNLIB_VAXMAG)
      CALL EPREAD(11,1,NW,IDAT,IBUF11,IERR)
      CALL EPMTREW(IERR)
#endif
      CALL EPREAD(11,1,NW,IDAT,IBUF11,IERR)
      IF(IERR.NE.4.AND.IERR.NE.5)  CALL ERREX(18)
      CALL EPREAD(11,1,NW,IDAT,IBUF11,IERR)
      IF(IERR.NE.1)  CALL ERREX(19)
      CALL EPRWND(11,IBUF11,IERR)
      IF(IERR.NE.0)  CALL ERREX(20)
#if (defined(CERNLIB_VAX))&&(!defined(CERNLIB_VAXMAG))
      CLOSE (11)
#endif
#if defined(CERNLIB_STF77VX)
      CLOSE (11)
#endif
#if defined(CERNLIB_STF77)
      CLOSE (11,STATUS='DELETE')
#endif
#if (defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_APOMAG))
C
C     FOR THE APOLLO THE STORY IS DIFFERENT. WE NEED TO CLOSE
C     THE STREAM ASSOCIATED WITH 'FOR011', AND THEN TO DELETE
C     THE FILE.
C
      CALL STREAM_$INQUIRE(
     1      STREAM_$IRM_STRID,
     2       STREAM_$NAME_UNCONDITIONAL,
     3        ATT_$REC,
     4         ERROR_$MASK,
     6          STATUS_$RETURNED)
      IF(STATUS_$RETURNED.NE.STATUS_$OK)
     1   CALL PFM_$SIGNAL(STATUS_$RETURNED)
C
C     THE FILE IS THERE
C
      CALL STREAM_$CLOSE(ATT_$REC,STATUS_$RETURNED)
      IF(STATUS_$RETURNED.NE.STATUS_$OK)
     1   CALL PFM_$SIGNAL(STATUS_$RETURNED)
C
C     THE STREAM HAS BEEN CLOSED
C
      CALL NAME_$DELETE_FILE(FILNAM,INT2(10),STATUS_$RETURNED)
      IF(STATUS_$RETURNED.NE.STATUS_$OK)
     1   CALL PFM_$SIGNAL(STATUS_$RETURNED)
C
C     THE FILE HAS BEEN DELETED
C
#endif
#if defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX)
C  Reset the blocksize for the main tests.
      CALL EPSETW(11,1,1800,IERR)
#endif
      RETURN
      END
