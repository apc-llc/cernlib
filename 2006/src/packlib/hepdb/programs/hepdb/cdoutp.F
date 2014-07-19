*
* $Id: cdoutp.F,v 1.2 1996/03/12 13:08:53 cernlib Exp $
*
* $Log: cdoutp.F,v $
* Revision 1.2  1996/03/12 13:08:53  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:28  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDOUTP
#include "hepdb/cdunit.inc"
#include "zebra/zunit.inc"
#if defined(CERNLIB_IBMMVS)
      DIMENSION     DISP(3)
      DIMENSION     SPACE(4)
      CHARACTER*8   CHUNIT
#endif
      CHARACTER*3   CHSTAT
      LOGICAL       IEXIST,ISOPEN
      CHARACTER*255 CHFILE
      CHARACTER*8   CHOPT
#include "hepdb/hdboptd.inc"
 
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_NEWLIB))
      CHUNIT = 'FAST'
      LUNIT  = 4
#endif
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_GSI))
      CHUNIT = 'SYSDA'
      LUNIT  = 5
#endif
 
      CALL KUGETS(CHFILE,LFILE)
      CALL KUGETC(CHOPT,LOPT)
 
#include "hepdb/hdboptc.inc"
*
*     Close previously opened file
*
      IF(IOPTC.NE.0) THEN
         INQUIRE(LFILCD,OPENED=ISOPEN)
         IF(.NOT.ISOPEN) THEN
            IF(LLOGCD.GT.3) WRITE(LPRTCD,*) 'CDOUTP. CHFILE has ',
     +         'not yet been redirected'
            RETURN
         ENDIF
         CLOSE(LFILCD)
      ENDIF
*
*     Switch back to previously opened file
*
      IF(IOPTS.NE.0) THEN
         INQUIRE(LFILCD,OPENED=ISOPEN)
         IF(.NOT.ISOPEN) THEN
            IF(LLOGCD.GT.3) WRITE(LPRTCD,*) 'CDOUTP. CHFILE has ',
     +         'not yet been redirected'
            RETURN
         ENDIF
      ELSE
         LOUTCD = LFILCD
         IQPRNT = LFILCD
      ENDIF
*
*     Switch CHFILE back to terminal
*
      IF(IOPTC.NE.0.OR.IOPTT.NE.0) THEN
         LOUTCD = LPRTCD
         IQPRNT = LPRTCD
      ELSE
 
#if defined(CERNLIB_UNIX)
#include "hepdb/cdfold.inc"
#endif
#if !defined(CERNLIB_UNIX)
      CALL CLTOU(CHFILE(1:LFILE))
#endif
*
*     Open the file
*
#if defined(CERNLIB_IBMVM)
         CALL CTRANS('.',' ',CHFILE,1,LFILE)
#endif
#if defined(CERNLIB_IBM)
         INQUIRE(FILE='/'//CHFILE(1:LFILE),EXIST=IEXIST)
#endif
#if !defined(CERNLIB_IBM)
         INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
#endif
         IF(IEXIST) THEN
            IF(IOPTR.EQ.0) THEN
               WRITE(LPRTCD,*) 'CDOUTP. file already exists - ',
     +            'use the R option to replace'
               RETURN
            ENDIF
            CHSTAT  = 'OLD'
         ELSE
            CHSTAT  = 'NEW'
         ENDIF
#if defined(CERNLIB_IBMVM)
         CALL CTRANS('.',' ',CHFILE,1,LFILE)
         CALL FILEINF(ISTAT,'LRECL',133,'RECFM','F')
         OPEN(LFILCD,FILE='/'//CHFILE(1:LFILE),ACCESS='SEQUENTIAL',
#endif
#if defined(CERNLIB_IBMMVS)
         CALL FILEINF(ISTAT,'DEVICE',CHUNIT(1:LUNIT),'TRK',1,
     +                'SECOND',1,'DIR',0,
     +                'RECFM','VBA','LRECL',137,'BLKSIZE',6233)
         OPEN(LFILCD,FILE='/'//CHFILE(1:LFILE),ACCESS='SEQUENTIAL',
     +      ACTION='READWRITE',
#endif
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_IBMMVS))
         OPEN(LFILCD,FILE=CHFILE(1:LFILE),ACCESS='SEQUENTIAL',
#endif
     +      STATUS=CHSTAT,FORM='FORMATTED',IOSTAT=IRC)
*
*     Was open successful?
*
         IF(IRC.EQ.0) THEN
            LOUTCD = LFILCD
            IQPRNT = LFILCD
         ELSE
            IF(LLOGCD.GT.-3) WRITE(LPRTCD,*) 'CDOUTP. error ',IRC,
     +         ' opening ',CHFILE(1:LFILE)
         ENDIF
 
      ENDIF
 
      END
