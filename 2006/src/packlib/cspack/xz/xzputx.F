*
* $Id: xzputx.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzputx.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZPUTX(LOCAL,REMOTE,LRECL,CHOPT,IRC)
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
      CHARACTER*4   CHOPL,CHOPR
      LOGICAL       LDIO,RDIO
#include "cspack/czopts.inc"
#include "cspack/czopen.inc"
      IRC = 0
      IF(LRECL.EQ.0) THEN
         IRC = -1
         PRINT *,'XZPUTX. the record length (in bytes) must be given'
         RETURN
      ENDIF
 
#if defined(CERNLIB_UNIX)
      LDIO = .TRUE.
      CHOPL = 'ID'
      IF(IOPTC.NE.0) CHOPL = 'CID'
#endif
#if !defined(CERNLIB_UNIX)
      LDIO = .FALSE.
      CHOPL = 'I'
#endif
 
*
*     Open local file, options Input
*
      NCHL = LENOCC(LOCAL)
      NCHR = LENOCC(REMOTE)
      IF(IOPTA.EQ.0) THEN
         IF(IDEBXZ.GE.3) PRINT *,'XZPUTX. open local file ',
     +      LOCAL(1:NCHL),' with LRECL ',LRECL,' options ',CHOPL
         CALL SZOPEN(LUNXZI,LOCAL(1:NCHL),LRECL,CHOPL,ISTAT)
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
         IF(IOPTR.NE.0) THEN
            CHOPR = 'OD'
            IF(IOPTC.NE.0) CHOPR = 'COD'
         ELSE
            CHOPR = 'NOD'
            IF(IOPTC.NE.0) CHOPR = 'NCOD'
         ENDIF
      ELSE
         RDIO = .FALSE.
         CHOPR = 'NO'
         IF(IOPTR.NE.0) CHOPR = 'O'
      ENDIF
 
*
*     Create remote file
*
      NODE           = CUNODE
      CHNODE(LUNXZO) = NODE
      JSKIN(LUNXZO)  = ISKIN
      JSKOUT(LUNXZO) = ISKOUT
      IF(IDEBXZ.GE.3) PRINT *,'XZPUTX. open remote file ',
     +   REMOTE(1:NCHR),' with LRECL ',LRECL,' options ',CHOPR
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
 
      NREC = 0
   20 CONTINUE
      NREC = NREC + 1
 
      IF(LDIO) THEN
         IREC = NREC
      ELSE
         IREC = 0
      ENDIF
 
      IF(RDIO) THEN
         JREC = NREC
      ELSE
         JREC = 0
      ENDIF
 
 
      CALL SZREAD(LUNXZI,IBUFF,IREC,LRECL,NGOT,' ',IRC)
      IF(IRC.EQ.0) THEN
         CALL XZRITE(LUNXZO,IBUFF,JREC,LRECL,' ',IRC)
         IF(IRC.EQ.0) THEN
            NR = NR + 1
            GOTO 20
         ELSEIF(IRC.GT.0) THEN
*
*    If we are in D/A mode, assume EOF if NR > 0
*
            IF((LDIO).AND.(NR.GT.0)) GOTO 98
            GOTO 97
         ENDIF
      ENDIF
 
  98  CONTINUE
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
   90 PRINT *, ' Cannot open remote file'
      IRC = 1
      GO TO 99
   95 PRINT *, ' Cannot open local file'
      IRC = 2
      GO TO 99
   97 PRINT *, 'Problem in transferring file'
      IRC = 3
      CLOSE(LUNXZI)
*
   99 END
