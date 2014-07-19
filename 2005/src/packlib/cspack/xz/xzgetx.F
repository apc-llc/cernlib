*
* $Id: xzgetx.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzgetx.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZGETX(LOCAL,REMOTE,LRECL,CHOPT,IRC)
*
*     Options:
*              A - local file has already been opened
*              R - replace file if it already exists
*
#include "cspack/zmach.inc"
#include "cspack/czunit.inc"
#include "cspack/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
#include "cspack/quest.inc"
      CHARACTER*(*) REMOTE,LOCAL
#if defined(CERNLIB_IBM)
      CHARACTER*80  CHFILE
#endif
      CHARACTER*12  NODE
      CHARACTER*8   DELTIM
      DIMENSION     IBUFF(8192)
      LOGICAL       LDIO,RDIO
      CHARACTER*4   CHOPL,CHOPR
#include "cspack/czopts.inc"
#include "cspack/czopen.inc"
      IRC = 0
      NCHL = LENOCC(LOCAL)
      NCHR = LENOCC(REMOTE)
#if defined(CERNLIB_UNIX)
      LDIO = .TRUE.
      IF(IOPTR.NE.0) THEN
         CHOPL = 'OD'
         IF(IOPTC.NE.0) CHOPL = 'COD'
      ELSE
         CHOPL = 'NOD'
         IF(IOPTC.NE.0) CHOPL = 'NCOD'
      ENDIF
#endif
#if !defined(CERNLIB_UNIX)
      LDIO = .FALSE.
      CHOPL = 'NO'
      IF(IOPTR.NE.0) CHOPL = 'O'
#endif
 
      IF(IDEBXZ.GE.1) PRINT *,'XZGETX. enter for ',
     +   'LOCAL,REMOTE,LRECL,CHOPT = ',
     +    LOCAL(1:NCHL),',',REMOTE(1:NCHR),',',LRECL,',',CHOPT
 
      IF(LRECL.EQ.0) THEN
         PRINT *,'XZGETX. the record length (in bytes) must be given'
         IRC = -1
         RETURN
      ENDIF
*
*     Open local file, options Output
*
      ISTAT = 0
      IF(IOPTA.EQ.0) THEN
         CALL SZOPEN(LUNXZO,LOCAL(1:NCHL),LRECL,CHOPL,ISTAT)
         IF(ISTAT.EQ.28.AND.IDEBXZ.GE.-3) PRINT *,'XZGETX. ',
     +      'local file already exists. ',
     +      'Specify option R to replace'
         IF(IDEBXZ.GE.3) PRINT *,'XZGETX. return code ',ISTAT,
     +      ' from SZOPEN for ',LOCAL(1:NCHL),' on unit ',LUNXZO
         IF(ISTAT.NE.0) GOTO 95
      ENDIF
*
*     Ask remote server what type of system it is running
*
      CHMAIL = 'XZIO :RSYS'
      CALL CZPUTA(CHMAIL,ISTAT)
      CALL CZGETA(CHMAIL,ISTAT)
      IF(INDEX(CHMAIL,'UNIX').NE.0) THEN
         RDIO = .TRUE.
         CHOPR = 'UD'
         IF(IOPTC.NE.0) CHOPR = 'CUD'
      ELSE
         RDIO = .FALSE.
         CHOPR = 'U'
      ENDIF
 
*
*     Check if remote file exists; try to get its record length
*
      NODE      = CUNODE
      CHNODE(LUNXZI) = NODE
      JSKIN(LUNXZI)  = ISKIN
      JSKOUT(LUNXZI) = ISKOUT
      CALL XZOPEN(LUNXZI,REMOTE(1:NCHR),NODE,LRECL,CHOPR,IRC)
      IF(IDEBXZ.GE.3) PRINT *,'XZGETX. return code ',ISTAT,
     +   ' from XZOPEN for ',REMOTE(1:NCHR),' on unit ',LUNXZI
      IF(IRC.NE.0) GOTO 90
*
*          Start transfer
*
      NR=0
      NWANT=LRECL
*
*     Start timer
*
      IF(IOPTS.NE.0) THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
      ENDIF
 
      NREC = 0
   20 NREC = NREC + 1
 
      IF(RDIO) THEN
         IREC = NREC
      ELSE
         IREC = 0
      ENDIF
 
      CALL XZREAD(LUNXZI,IBUFF,IREC,NWANT,NGOT,' ',IRC)
      IF(IRC.EQ.0) THEN
         NR = NR + 1
         NWR = NWANT/IQCHAW
 
         IF(LDIO) THEN
            JREC = NREC
         ELSE
            JREC = 0
         ENDIF
 
         CALL SZRITE(LUNXZO,IBUFF,JREC,NWANT,' ',IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBXZ.GE.0) PRINT *,'XZGETX. error ',IRC, ' from '
     +      //'SZRITE'
            GOTO 97
         ENDIF
         GOTO 20
      ELSEIF(IRC.GT.0) THEN
         IF((RDIO).AND.(NR.GT.0)) GOTO 98
         IF(IDEBXZ.GE.0) PRINT *,'XZGETX. error ',IRC,
     +      ' from XZREAD'
         GOTO 97
      ENDIF
 
   98 CONTINUE
 
      CLOSE(LUNXZO)
      CALL XZCLOS(LUNXZI,' ',IRC)
      IF(IDEBXZ.GE.0) PRINT *,' File transfer completed'
      NKILO = NR*LRECL / 1024
      IF(IOPTS.NE.0)THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
         READ(DELTIM,'(I2,1X,I2,1X,I2)') IHOUR,IMIN,ISEC
         NSECS = ISEC + IMIN*60 + IHOUR*3600
         IF(NSECS.LE.0) NSECS = 1
         RATE  = FLOAT(NKILO)/FLOAT(NSECS)
#include "cspack/xzstat.inc"
         PRINT *,' Transferred ',NR,' records, transfer rate = ',RATE,
     +           ' KB/S'
         PRINT *,' Elapsed time = ',DELTIM,' CP time = ',T,' sec.'
      ENDIF
      GO TO 99
*
*          Error
*
   90 PRINT *, ' Cannot open remote file'
      IRC = 1
      GO TO 99
   95 PRINT *, ' Cannot open local file'
      IRC = 2
      GO TO 99
   97 PRINT *, 'Problem in transferring file'
      IRC = 3
      CLOSE(LUNXZO)
*
   99 END
