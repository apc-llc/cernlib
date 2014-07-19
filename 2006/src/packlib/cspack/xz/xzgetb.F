*
* $Id: xzgetb.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzgetb.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZGETB(LOCAL,REMOTE,LRECL,CHOPT,IRC)
*
*     Options:
*              A - local file has already been opened
*              R - replace local file if it already exists
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
      CHARACTER*4   CHOPI
      DIMENSION IBUFF(8192)
      PARAMETER (NREC=0)
#include "cspack/czopts.inc"
#include "cspack/czopen.inc"
      IRC = 0
      NCHL = LENOCC(LOCAL)
      NCHR = LENOCC(REMOTE)
 
      IF(IDEBXZ.GE.1) PRINT *,'XZGETB. enter for ',
     +   'LOCAL,REMOTE,LRECL,CHOPT = ',
     +    LOCAL(1:NCHL),',',REMOTE(1:NCHR),',',LRECL,',',CHOPT
 
      IF(LRECL.EQ.0) THEN
         PRINT *,'XZGETB. the record length (in bytes) must be given'
         IRC = -1
         RETURN
      ENDIF
*
*     Open local file, options Output
*
      ISTAT = 0
      IF(IOPTA.EQ.0) THEN
 
         IF(IOPTR.EQ.0) THEN
            CHOPI = 'NO'
            IF(IOPTC.NE.0) CHOPI = 'NOC'
         ELSE
            CHOPI = 'O'
            IF(IOPTC.NE.0) CHOPI = 'OC'
         ENDIF
 
         CALL SZOPEN(LUNXZO,LOCAL(1:NCHL),LRECL,CHOPI,ISTAT)
         IF(ISTAT.EQ.28.AND.IDEBXZ.GE.-3) PRINT *,'XZGETB. ',
     +      'local file already exists. ',
     +      'Specify option R to replace'
         IF(IDEBXZ.GE.3) PRINT *,'XZGETB. return code ',ISTAT,
     +      ' from SZOPEN for ',LOCAL(1:NCHL),' on unit ',LUNXZO
         IF(ISTAT.NE.0) GOTO 95
      ENDIF
*
*     Check if remote file exists; try to get its record length
*
      NODE      = CUNODE
      CHNODE(LUNXZI) = NODE
      JSKIN(LUNXZI)  = ISKIN
      JSKOUT(LUNXZI) = ISKOUT
      IF(IOPTC.EQ.0) THEN
         CALL XZOPEN(LUNXZI,REMOTE(1:NCHR),NODE,LRECL,'U',IRC)
      ELSE
         CALL XZOPEN(LUNXZI,REMOTE(1:NCHR),NODE,LRECL,'UC',IRC)
      ENDIF
      IF(IDEBXZ.GE.3) PRINT *,'XZGETB. return code ',IRC,
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
 
   20 CALL XZREAD(LUNXZI,IBUFF,NREC,NWANT,NGOT,' ',IRC)
      IF(IRC.EQ.0) THEN
         NR = NR + 1
         CALL SZRITE(LUNXZO,IBUFF,0,NWANT,' ',IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBXZ.GE.0) PRINT *,'XZGETB. error ',IRC, ' from '
     +      //'SZRITE'
            GOTO 97
         ENDIF
         GOTO 20
      ELSEIF(IRC.GT.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZGETB. error ',IRC,
     +      ' from XZREAD'
         GOTO 97
      ENDIF
 
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
   90 PRINT *, 'Cannot open remote file'
      IRC = 1
      GO TO 99
   95 PRINT *, 'Cannot open local file'
      IRC = 2
      GO TO 99
   97 PRINT *, 'Problem in transferring file'
      IRC = 3
      CLOSE(LUNXZO)
*
   99 END
