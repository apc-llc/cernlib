*
* $Id: fmdumc.F,v 1.1.1.1 1996/03/07 15:17:42 mclareni Exp $
*
* $Log: fmdumc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:42  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMDUMC
*
*     Request tape dump of VID corresponding to specified generic name
*
#include "fatmen/fmpath.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatsys.inc"
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      CHARACTER*255 GENAM,GENAM1
      CHARACTER*8   CODE
#include "fatmen/fatinit.inc"
      CALL KUGETC(GENAM,LGN)
      CALL FMFIXF(GENAM,GENAM1)
      LGN = LENOCC(GENAM1)
      GENAM = GENAM1
      CALL KUGETI(IKEY)
      CALL KUGETI(NBYTES)
      CALL KUGETI(NBLOCKS)
      CALL KUGETI(NFILES)
      CALL KUGETC(CODE,NC)
      CALL VZERO(KEYS,10)
      KEYS(1) = IKEY
      CALL FMDUMP(GENAM(1:LGN),KEYS,NBYTES,NBLOCKS,NFILES,CODE,IRC)
      IF(IRC.NE.0) PRINT *,'Return code ',IRC,' from FMDUMP'
 
      END
 
