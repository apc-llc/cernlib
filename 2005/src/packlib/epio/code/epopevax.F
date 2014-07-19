*
* $Id: epopevax.F,v 1.1.1.1 1996/03/08 15:21:44 mclareni Exp $
*
* $Log: epopevax.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:44  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_VAX)
      SUBROUTINE EPOPEN_VAX(MODE,IERR)
      IMPLICIT INTEGER (A-Z)
      INCLUDE   '($DCDEF)'
      INCLUDE   '($SSDEF)'
      INCLUDE   '($FORIOSDEF)'
#include "epio/epiocom.inc"
#include "epio/epvaxcom.inc"
      CHARACTER*6 STATUS
C
C     Determines if file is on magtape or disk and opens it accordingly
C
C     Input:
C
C             MODE     =1 for writing
C                      =2 for reading
C
C      Mods by Achille Petrilli, Federico Carminati
C
      CHARACTER*10 RTYPE, CR
      INTEGER*2   IDESC2(8)
      INTEGER     IDESC4(4)
      EQUIVALENCE (IDESC2,IDESC4)
      LOGICAL     OPEN,EXI
      EXTERNAL EPOPENS_VAX,EPOPENV_VAX,EPOPENO_VAX,MT$K_NORMAL15
      EXTERNAL DVI$_DEVCLASS
C
C        Fill the item list
C
      IDESC2(1)=4
      IDESC2(2)=%LOC(DVI$_DEVCLASS)
      IDESC4(2)=%LOC(IDEVIC)
      IDESC4(3)=%LOC(DEVLEN)
      IDESC4(4)=0
C
      LUN  = LIST(ISTART+10)
      LREC = (LIST(ISTART+1)+1)/2
C
C      USE USER SPECIFIED NAME, OR DEFAULT NAME 'FOR0NN'
C
      IF (EPVXUN(LREF)(1:1).EQ.' ')
     +              WRITE(EPVXUN(LREF),'(''FOR'',I3.3)') LUN
      NSLNG = LENOCC(EPVXUN(LREF))
C
      INQUIRE(LUN,OPENED=OPEN)
      IF(OPEN) GO TO 999
      LIST(ISTART+24) = 0
      ISTAT=SYS$GETDVIW(,,EPVXUN(LREF)(1:NSLNG),IDESC4,,,,)
      IF (ISTAT.EQ.SS$_IVDEVNAM.OR.ISTAT.EQ.SS$_NOSUCHDEV
     +    .OR.ISTAT.EQ.SS$_NONLOCAL.OR.(ISTAT.EQ.SS$_NORMAL.AND.
     +    IDEVIC.EQ.DC$_DISK)) THEN
C
C     The error codes SS$_IVDEVNAM & SS$_NOSUCHDEV & SS$_NONLOCAL
C     mean this is a DISK file because:
C
C     SS$_IVDEVNAM is returned when the file name is like
C     ABC.DAT[;N] ("." & ";" are illegal in device names)
C
C     SS$_NOSUCHDEV is returned when the file name is like
C     FOR010 (no assignment to FOR010, interpreted as
C     FOR010.DAT by FORTRAN, legal but inexistent device name)
C
C     SS$_NONLOCAL is returned in case of a DECNET node. in this
C     case we always assume a disk device
C
         IDEVIC=DC$_DISK
         IF (MODE.EQ.2) THEN
            INQUIRE(FILE=EPVXUN(LREF)(1:NSLNG),EXIST=EXI)
            IF (.NOT.EXI) GO TO 998
C
            OPEN (UNIT=LUN,FILE=EPVXUN(LREF)(:NSLNG),STATUS='OLD',
     1            READONLY,FORM='UNFORMATTED',SHARED)
            INQUIRE(UNIT=LUN,RECORDTYPE=RTYPE,CARRIAGECONTROL=CR)
            READ(LUN,IOSTAT=IOS)
            IF(IOS.EQ.FOR$IOS_SEGRECFOR) RTYPE='VARIABLE'
            CLOSE(LUN)
C
C                      AND A READ
C
            OPEN (UNIT=LUN,FILE=EPVXUN(LREF)(:NSLNG),STATUS='OLD',
     1            READONLY,FORM='UNFORMATTED',CARRIAGECONTROL=CR,
     2            SHARED,RECORDTYPE=RTYPE,USEROPEN=EPOPENV_VAX)
         ELSE
C
C                      And a write
C
            IF(LIST(ISTART+30).EQ.0) THEN
               STATUS = 'NEW'
            ELSEIF(LIST(ISTART+30).EQ.1) THEN
               STATUS='APPEND'
            END IF
            OPEN (UNIT=LUN,NAME=EPVXUN(LREF)(:NSLNG),TYPE=STATUS,
     1      FORM='UNFORMATTED',RECORDTYPE='FIXED',RECL = LREC,
     2      BLOCKSIZE = LREC*4,USEROPEN=EPOPENO_VAX)
         ENDIF
C
      ELSEIF(IDEVIC.EQ.DC$_TAPE) THEN
C
C     This is a magtape; the EPOPENS_VAX user open routine will
C     put the channel number into LIST(ISTART+25)
C
          IF(MODE.EQ.2)  THEN
C
C                      And a read
C
              OPEN (UNIT=LUN,NAME=EPVXUN(LREF)(:NSLNG),TYPE='OLD',
     1              FORM='UNFORMATTED', RECORDTYPE = 'FIXED',
     3              USEROPEN = EPOPENS_VAX)
          ELSE
C
C                      And a write
C
              OPEN (UNIT=LUN,NAME=EPVXUN(LREF)(:NSLNG),TYPE='NEW',
     1              FORM='UNFORMATTED',RECORDTYPE='FIXED',
     2              RECL = LREC,   BLOCKSIZE = LREC*4,
     3              USEROPEN = EPOPENS_VAX)
          ENDIF
C
C     Set the swap mode: this routine switches the hardware
C     in the mag tape formatter and must be handled with care
C     for further information contact A.Rothenberg or M.Scire
C
          IERR = MT_FORMAT(LIST(ISTART+25),MT$K_NORMAL15)
C
      ELSE
C
C         This is an illegal device !!!
C
          GO TO 998
C
      ENDIF
      GO TO 999
 998   IERR=1
 999  END
#endif
