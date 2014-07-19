*
* $Id: xzputb.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzputb.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZPUTB(LOCAL,REMOTE,LRECL,CHOPT,IRC)
*              A - local file has already been opened
#include "cspack/zmach.inc"
#include "cspack/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/czunit.inc"
#include "cspack/czbuff.inc"
#include "cspack/quest.inc"
      CHARACTER*(*) LOCAL,REMOTE
      CHARACTER*12  NODE
      CHARACTER*8   DELTIM
      DIMENSION     IBUFF(8192)
      PARAMETER     (NREC=0)
      CHARACTER*4   CHOPR
#include "cspack/czopts.inc"
#include "cspack/czopen.inc"
      IRC = 0
      IF(LRECL.EQ.0) THEN
         IRC = -1
         PRINT *,'XZPUTB. the record length (in bytes) must be given'
         RETURN
         ENDIF
*
*     Open local file, options Input
*
      NCHL = LENOCC(LOCAL)
      NCHR = LENOCC(REMOTE)
      IF(IOPTA.EQ.0) THEN
         IF(IOPTC.EQ.0) THEN
            CALL SZOPEN(LUNXZI,LOCAL(1:NCHL),LRECL,'I',ISTAT)
         ELSE
            CALL SZOPEN(LUNXZI,LOCAL(1:NCHL),LRECL,'IC',ISTAT)
         ENDIF
         IF(ISTAT.NE.0) GOTO 95
      ENDIF
*
*     Create remote file
*
      NODE           = CUNODE
      CHNODE(LUNXZO) = NODE
      JSKIN(LUNXZO)  = ISKIN
      JSKOUT(LUNXZO) = ISKOUT
      CHOPR     = 'ON'
      IF(IOPTR.NE.0) CHOPR = 'O'
      IF(IOPTC.NE.0) THEN
         LCH = LENOCC(CHOPR) + 1
         CHOPR(LCH:LCH) = 'C'
      ENDIF
      CALL XZOPEN(LUNXZO,REMOTE(1:NCHR),NODE,LRECL,CHOPR,IRC)
      IF(IRC.NE.0) GOTO 90
*
*          Start transfer
*
      NR=0
*
*     Start timer
*
      IF(IOPTS.NE.0) THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
         ENDIF
 
  20  NWORDS=LRECL/IQCHAW
      CALL XINBF(LUNXZI,IBUFF,NWORDS)
      IF(NWORDS.GT.0) THEN
         CALL XZRITE(LUNXZO,IBUFF,NREC,LRECL,' ',IRC)
         IF(IRC.EQ.0) THEN
            NR = NR + 1
            GOTO 20
            ELSEIF(IRC.GT.0) THEN
            GOTO 97
            ENDIF
         ENDIF
 
      CLOSE(LUNXZI)
      CALL XZCLOS(LUNXZO,' ',IRC)
      IF(IDEBXZ.GE.0) PRINT *,' File transfer completed'
      IF(IOPTS.NE.0)THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
         READ(DELTIM,'(I2,1X,I2,1X,I2)') IHOUR,IMIN,ISEC
         NSECS = ISEC + IMIN*60 + IHOUR*3600
         IF(NSECS.LE.0) NSECS = 1
         NKILO = NR*LRECL / 1024
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
  90  PRINT *, ' Cannot open remote file'
      IRC = 1
      GO TO 99
  95  PRINT *, ' Cannot open local file'
      IRC = 2
      GO TO 99
  97  PRINT *, 'Problem in transferring file'
      IRC = 3
      CLOSE(LUNXZI)
*
  99  END
 
