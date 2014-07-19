*
* $Id: kaxglv.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxglv.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXGLV(LOC)
*
*.....GENERATE A NEW TREE LEVEL, (TO AVOID SPLITTING A ROOT BLOCK)
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax0a0.inc"
#include "kapack/kax0b0.inc"
*
*-----------------------------------------------------------------------
*
*.....GET A NEW BLOCK
      CALL KAXGBK(NBLOCK)
*
*.....RE-FORMAT THE BLOCK CONTROL WORDS OF THE ROOT BLOCK
      CALL UCOPY(IA,IB,IA(6)-1)
      IB(5) = IB(5) + 1
      IB(7) = IB(6) + 4
*
*.....CONSTRUCT AN INDEX ENTRY POINTING TO THE NEW BLOCK
      I = IB(6)
      IB(I+0) = 4
      IB(I+1) = 2
      IB(I+2) = MAXKEY
      IB(I+3) = NBLOCK
*
*.....UPDATE THE CURRENT BLOCK
      IA(1) = NBLOCK
*
*.....CONTRACT THE BLOCK CONTROL WORDS IF BLOCK 1 IS BEING REPLACED
      IF ( IB(1) .EQ. 1 )  THEN
         CALL UCOPY( IA(NBCW1+1), IA(NBCW+1), IA(7)-IA(6) )
         IA(6) = IA(6) - (NBCW1-NBCW)
         IA(7) = IA(7) - (NBCW1-NBCW)
         LOC   = LOC   - (NBCW1-NBCW)
         IA(2) = 0
         IA(3) = 0
      ENDIF
*
*.....WRITE THE BLOCKS
      CALL KAXWRT(IA(1),IA,IA(7)-1)
      CALL KAXWRT(IB(1),IB,IB(7)-1)
*
      END
