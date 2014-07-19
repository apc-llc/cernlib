*
* $Id: fmupky.F,v 1.1.1.1 1996/03/07 15:18:10 mclareni Exp $
*
* $Log: fmupky.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:10  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMUPKY(GENAME,LBANK,KEYS,IRC)
      CHARACTER*(*) GENAME
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      LGN = LENOCC(GENAME)
*
*     Fill KEYS vector from bank
*
      IRC          = 0
      LPATH        = INDEXB(GENAME(1:LGN),'/')
      CALL VBLANK(KEYS(2),5)
      CALL UCTOH(GENAME(LPATH+1:LGN),KEYS(2),4,LGN-LPATH)
      KEYS(MKSRFA) = 0
      KEYS(MKCLFA) = IQ(LBANK+KOFUFA+MCPLFA)
      KEYS(MKLCFA) = IQ(LBANK+KOFUFA+MLOCFA)
      KEYS(MKMTFA) = IQ(LBANK+KOFUFA+MMTPFA)
      KEYS(MKNBFA) = MKNBFA
      END
 
