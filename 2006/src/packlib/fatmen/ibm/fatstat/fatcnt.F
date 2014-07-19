*
* $Id: fatcnt.F,v 1.1.1.1 1996/03/07 15:17:36 mclareni Exp $
*
* $Log: fatcnt.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:36  mclareni
* Fatmen
*
*
#if defined(CERNLIB_IBMVM)
#include "sys/CERNLIB_machine.h"
#include "_fatmen/pilot.h"
      SUBROUTINE FATCNT(LUNRZ)
      DIMENSION IBUF(1024)
      CHARACTER*132 LINE
      NZERO = 0
      NLAST = 0
      CALL VMCMS('LISTFILE CERN FATRZ * (L STACK',IRC)
      CALL VMRTRM(LINE,LEN)
      READ(LINE,'(34X,I10)') NREC
      PRINT 9001,NREC
      DO 10 IREC=1,NREC
      READ(UNIT=LUNRZ,REC=IREC,ERR=30) IBUF
*
*     Check which is the last non-zero record
*     Count total number of zeroed records
*
      DO 20 I=1,1024
      IF(IBUF(I).NE.0) THEN
         NLAST = IREC
         GOTO 10
         ENDIF
20    CONTINUE
      NZERO = NZERO + 1
10    CONTINUE
      PRINT 9002,NZERO,NREC-NZERO,NLAST
      RETURN
30    CONTINUE
      PRINT 9003,IREC
9001  FORMAT(' FATCNT. Total of ',I5,' records in CERN FATRZ')
9002  FORMAT(' FATCNT. Total of ',I5,' zeroed records, number used = ',
     +I5,' last used = ',I5)
9003  FORMAT(' FATCNT. Error at record ',I5)
      RETURN
      END
#endif
