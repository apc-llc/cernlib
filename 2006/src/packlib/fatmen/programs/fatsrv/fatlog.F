*
* $Id: fatlog.F,v 1.1.1.1 1996/03/07 15:17:41 mclareni Exp $
*
* $Log: fatlog.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:41  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FATLOG(IHEAD,LHEAD,IFORM,LFORM,IRC)
#include "fatmen/fatsys.inc"
#include "zebra/quest.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
      DIMENSION    IHEAD(LHEAD),IFORM(LFORM)
      CHARACTER*21 CHLOG
      PARAMETER    (MEDIUM=0)
      PARAMETER    (NWREC=900)
      PARAMETER    (NBUF=0)
      PARAMETER    (LUNLOG=11)
      INTEGER      CHDIRF
 
      IRC   = 0
 
#if !defined(CERNLIB_IBM)
      ISTAT = CHDIRF(DEFAULT)
*
*     Write logging record to FZ file
*
      CALL DATIME(ID,IT)
      WRITE(CHLOG,9001) ID
 9001 FORMAT('fatlogs/FATLOG.',I6)
 
      IF(IDEBFA.GE.1) PRINT 9002,CHLOG
 9002 FORMAT(' FATLOG. writing log record to ',A)
 
      CALL CFOPEN(LUNPTR,MEDIUM,NWREC,'a+',NBUF,CHLOG,IRC)
      IF(IRC.NE.0) THEN
         PRINT 9003,IRC
 9003 FORMAT(' FATLOG. cannot open log file: ',A,' irc = ',I5)
         GOTO 999
      ENDIF
      IQUEST(1) = LUNPTR
 
      CALL FZFILE(LUNLOG,NWREC,'LXO')
      IF(IQUEST(1).NE.0) THEN
         PRINT 9004,IQUEST(1)
         IRC = IQUEST(1)
 9004 FORMAT(' FATLOG. return code ',I5,' from FZFILE')
         GOTO 999
      ENDIF
 
      CALL FZOUT(LUNLOG,0,0,1,'Z',IFORM,LHEAD,IHEAD)
      IF(IQUEST(1).NE.0) THEN
         PRINT 9005,IQUEST(1)
         IRC = IQUEST(1)
 9005 FORMAT(' FATLOG. return code ',I5,' from FZOUT')
         GOTO 999
      ENDIF
 
      CALL FZENDO(LUNLOG,'TX')
#endif
 
  999 END
