*
* $Id: fmtchc.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmtchc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMTCHC
*
*     'TOUCH' files
*
#include "fatmen/fatsys.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      CHARACTER*255 GENAM,CHDIR
      CHARACTER*8   CHOPT
#include "fatmen/fatinit.inc"
 
      CALL RZCDIR(CHDIR,'R')
      LDIR = LENOCC(CHDIR)
      CALL KUGETC(GENAM,LGN)
      CALL FMFIXF(GENAM,GENAM)
      LGN = LENOCC(GENAM)
      CALL KUGETI(KSN)
      CALL KUGETC(CHOPT,NCH)
*
*     If KSN = 0, FMTOUC will TOUCH the file 'chosen' by FATMEN
*
      CALL VZERO(KEYS,LKEYFA)
      KEYS(1) = KSN
      CALL FMTOUC(GENAM(1:LGN),KEYS,CHOPT,IRC)
      IF(IRC.NE.0) PRINT *,'Return code ',IRC,' from FMTOUC'
 
      CALL RZCDIR(CHDIR(1:LDIR),' ')
 
      END
 
