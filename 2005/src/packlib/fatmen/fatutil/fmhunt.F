*
* $Id: fmhunt.F,v 1.2 1996/04/02 12:04:16 jamie Exp $
*
* $Log: fmhunt.F,v $
* Revision 1.2  1996/04/02 12:04:16  jamie
* save IQSAVE (for HP)
*
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMHUNT(GENAM,FILE,KEYS,CHOPT,IRC)
*CMZ :          12/11/91  08.46.38  by  Jamie Shiers
*-- Author :    Jamie Shiers   12/11/91
*
* Input:
*   GENAM   The (wild-carded) generic name to hunt for
*   CHOPT   Options
*
* Output:
*
*   FILE    Full generic name of matching file (if any)
*   KEYS    KEYS vector of matching file
*
*           IQUEST(10) = number levels in initial path
*           IQUEST(12) = number of subdirectories at this level
*           IQUEST(13) = number of elements selected pathname
*           IQUEST(14) = number of files in selected directory
*           IQUEST(15) = number of words per key
*           IQUEST(17) = number of this key vector
*
      CHARACTER*(*) GENAM,FILE
      CHARACTER*255 CHCWD
      DIMENSION     IQSAVE(10)
      SAVE          IQSAVE
#include "zebra/quest.inc"
#include "fatmen/fahucm.inc"
#include "fatmen/fatbug.inc"
      DIMENSION     KEYS(LKEYFA)
      EXTERNAL      FAHUNT
#include "fatmen/fatopts.inc"
 
      IRC = 0
      LGN = LENOCC(GENAM)
      CALL FACDIR(CHCWD,'R')
      LCD = LENOCC(CHCWD)
      CHFILE = ' '
      IF(IDEBFA.GE.1) PRINT *,'FMHUNT. enter for ',GENAM(1:LGN)
 
      CALL FMLOOP(GENAM(1:LGN),-1,FAHUNT,IRC)
      CALL UCOPY(IQUEST(10),IQSAVE(10),10)
 
      CALL FACDIR(CHCWD(1:LCD),' ')
 
      LFI = LENOCC(CHFILE)
      IF(LFI.GT.0) THEN
         FILE = CHFILE(1:LFI)
         CALL UCOPY(IQSAVE(10),IQUEST(10),10)
         CALL UCOPY(KEYH,KEYS,LKEYFA)
         IRC = 0
      ELSE
         IRC = 1
      ENDIF
 
      END
