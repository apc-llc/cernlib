*
* $Id: fatplog.F,v 1.1.1.1 1996/03/07 15:17:39 mclareni Exp $
*
* $Log: fatplog.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:39  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
*     Print FATMEN log records
      PROGRAM FATPLOG
      COMMON/PAWC/PAW(50000)
      COMMON/QUEST/ IQUEST(100)
      CHARACTER*255 GENAM
      CHARACTER*4   CHCOMM
      DIMENSION     IUHEAD(400)
      DIMENSION     IOCR(100)
      PARAMETER     (JBIAS=2)
      PARAMETER     (IEVENT=0)
 
      CALL HLIMIT(50000)
 
      CALL FZFILE(1,0,'ID')
      OPEN(1,ACCESS='DIRECT',
     +    RECL=3600,
     +    FORM='UNFORMATTED',
     +    STATUS='OLD')
      CALL FZLOGL(1,0)
      NREC  = 0
   1  CONTINUE
      NHEAD = 400
      CALL FZIN(1,0,LSUP,2,'S',NHEAD,IUHEAD)
      NREC = NREC + 1
      PRINT *,'Record ',NREC,' header ',NHEAD,' words'
      IF(NHEAD.EQ.170) CALL FMPLOG(6,IUHEAD,NHEAD,IRC)
      IF(IQUEST(1).LT.4) GOTO 1
      PRINT *,IQUEST(1)
      CALL FZENDI(1,'T')
*
  99  END
 
 
