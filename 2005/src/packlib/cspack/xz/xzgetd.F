*
* $Id: xzgetd.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzgetd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZGETD(LOCAL,REMOTE,LRECL,CHOPT,IRC)
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
      DIMENSION IBUFF(8192)
      CHARACTER*4   CHOPI
#include "cspack/czopts.inc"
#include "cspack/czopen.inc"
      IRC = 0
      IF(LRECL.EQ.0) THEN
         PRINT *,'XZGETD. the record length (in bytes) must be given'
         IRC = -1
         RETURN
      ENDIF
*
*     Open local file, options Output
*
      NCHL = LENOCC(LOCAL)
      NCHR = LENOCC(REMOTE)
      IF(IDEBXZ.GE.1)
     +PRINT *,'XZGETD. enter for Local = ',LOCAL(1:NCHL),
     +        ' Remote = ',REMOTE(1:NCHR),' LRECL = ',LRECL,
     +        ' CHOPT = ',CHOPT
      IF(IOPTA.EQ.0) THEN
         CHOPI = 'DNO'
         IF(IOPTR.NE.0) CHOPI = 'DO'
         IF(IOPTC.NE.0) THEN
            LCH = LENOCC(CHOPI) + 1
            CHOPI(LCH:LCH) = 'C'
         ENDIF
         CALL SZOPEN(LUNXZO,LOCAL(1:NCHL),LRECL,CHOPI,ISTAT)
         IF(ISTAT.EQ.28.AND.IDEBXZ.GE.-3) PRINT *,'XZGETD. ',
     +      'local file already exists. ',
     +      'Specify option R to replace'
 
         IF(ISTAT.NE.0) GOTO 95
      ENDIF
*
*     Check if remote file exists; try to get its record length
*
      NODE           = CUNODE
      CHNODE(LUNXZI) = NODE
      JSKIN(LUNXZI)  = ISKIN
      JSKOUT(LUNXZI) = ISKOUT
      IF(IOPTC.EQ.0) THEN
         CALL XZOPEN(LUNXZI,REMOTE(1:NCHR),NODE,LRECL,'DU',IRC)
      ELSE
         CALL XZOPEN(LUNXZI,REMOTE(1:NCHR),NODE,LRECL,'DUC',IRC)
      ENDIF
      IF(IRC.NE.0) GOTO 90
*
*          Start transfer
*
      NR=0
      NREC = 0
      NWANT=LRECL
*
*     Start timer
*
      IF(IOPTS.NE.0) THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
      ENDIF
 
   20 NREC = NREC + 1
      CALL XZREAD(LUNXZI,IBUFF,NREC,NWANT,NGOT,' ',IRC)
      IF(IRC.EQ.0) THEN
         NR = NR + 1
         NWR = NWANT/IQCHAW
         CALL SZRITE(LUNXZO,IBUFF,NREC,LRECL,' ',IRC)
         GOTO 20
      ELSEIF(IRC.GT.0) THEN
*
*     For D/A files, cannot distinguish between read error
*     and EOF. Assume EOF if >0 records read.
*
         IF(NR.EQ.0) GOTO 97
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
 
