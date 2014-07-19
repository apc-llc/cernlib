*
* $Id: fmkdir.F,v 1.2 1998/02/13 13:59:33 jamie Exp $
*
* $Log: fmkdir.F,v $
* Revision 1.2  1998/02/13 13:59:33  jamie
* set lbank to 0 - no bank involved
*
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMKDIR(GENAM,IRC)
#include "fatmen/faust.inc"
#include "zebra/quest.inc"
      CHARACTER*(*) GENAM
      CHARACTER*4 COMM
#include "fatmen/fatbug.inc"
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(10)
 
      NFMDIR = NFMDIR + 1
 
      COMM = 'MDIR'
      LGN  = LENOCC(GENAM)
      IRC  = 0
      LBANK= 0
      CALL FMVERI(GENAM,LBANK,KEYS,'G',IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GT.-3) PRINT *,'FMKDIR. errors detected by FMVERI - '
     +                           ,'entry will not be added to catalogue'
         RETURN
      ENDIF
 
      CALL FMFZO(COMM,GENAM(1:LGN),0,KEYS,IRC)
*
*     Check if this directory already exists
*
      CALL FACDIR(GENAM(1:LGN),' ')
      IQUEST(12) = IQUEST(1)
 
      RETURN
      END
