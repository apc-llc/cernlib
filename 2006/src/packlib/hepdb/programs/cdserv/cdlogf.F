*
* $Id: cdlogf.F,v 1.2 1996/03/12 13:08:02 cernlib Exp $
*
* $Log: cdlogf.F,v $
* Revision 1.2  1996/03/12 13:08:02  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLOGF(ICODE,TEXT)
      CHARACTER*(*) TEXT
#include "hepdb/cdnamc.inc"
#include "hepdb/slate.inc"
      CHARACTER*10  CHACC
      PARAMETER     (LUNLG=3)
      DATA          NENTRY/0/
      SAVE          NENTRY,IOPLOG
 
      LTEXT = LENOCC(TEXT)
      IF (IOPLOG.EQ.0) THEN
         IERR = 0
         CHACC = 'APPEND'
#if defined(CERNLIB_IBMVM)
   10    OPEN(UNIT=LUNLG,STATUS='UNKNOWN',IOSTAT=ISTAT,
     +        FORM='FORMATTED',FILE='/HEPDB LOG '//CDLOGD(1:LLOGD))
#endif
#if defined(CERNLIB_APOLLO)
   10    OPEN(UNIT=LUNLG,STATUS='APPEND',IOSTAT=ISTAT,
#endif
#if defined(CERNLIB_VAXVMS)
   10    OPEN(UNIT=LUNLG,ACCESS=CHACC,IOSTAT=ISTAT,STATUS='UNKNOWN',
     +        SHARED,
     +        FORM='FORMATTED',FILE=CDLOGD(1:LLOGD)//'hepdb.log')
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))
   10    OPEN(UNIT=LUNLG,STATUS='UNKNOWN',IOSTAT=ISTAT,
#endif
#if defined(CERNLIB_UNIX)
     +        FORM='FORMATTED',FILE=CDLOGD(1:LLOGD)//'/hepdb.log')
#endif
         IF(ISTAT.NE.0) THEN
            IC = SLEEPF(1)
            IERR = IERR + 1
            IF(IERR.GT.10) THEN
               CALL DATIME(ID,IT)
               PRINT 9003,ICODE,(IS(I),I=1,6),TEXT(1:LTEXT)
               STOP 'Stop because the log file is locked'
            ELSE
               GO TO 10
            ENDIF
         ENDIF
         IOPLOG = 1
#if defined(CERNLIB_VAXVMS)
*
*      Try a dummy write to trap problem with DEC Fortran 6.1
*
         IF(CHACC.EQ.'APPEND') THEN
            WRITE (LUNLG,'(A)',IOSTAT=ISTAT)
            IF(ISTAT.NE.0) THEN
               PRINT 9001,ISTAT,LUNLG,CHACC
 9001 FORMAT(' CDLOGF. error ',I6,' writing to file on unit ',I6,
     +       ' using ACCESS=',A)
               PRINT 9002
 9002 FORMAT(' CDLOGF. switching to ACCESS=SEQUENTIAL')
               CLOSE (LUNLG)
               CHACC = 'SEQUENTIAL'
               GOTO 10
            ENDIF
         ENDIF
#endif
      ENDIF
      CALL DATIME(ID,IT)
      WRITE (LUNLG,9003) ICODE,(IS(I),I=1,6),TEXT(1:LTEXT)
      IF(TEXT(1:4).EQ.'Stop') THEN
         CLOSE(LUNLG)
         IOPLOG = 0
      ENDIF
 9003 FORMAT(I4,1X,I4.4,'/',I2.2,'/',I2.2,'/',1X,
     +       I2.2,':',I2.2,':',I2.2,1X,A)
      END
