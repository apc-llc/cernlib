*
* $Id: kaxdlv.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxdlv.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXDLV
*
*.....DELETE A TREE LEVEL, (THE ROOT BLOCK CONTAINS ONLY ONE ENTRY)
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax0a0.inc"
#include "kapack/kax0b0.inc"
*
*-----------------------------------------------------------------------
*
*.....READ THE BLOCK POINTED TO BY THE ROOT BLOCK ENTRY
      LOC    = IA(6)
      IPTR   = LOC + NRCW + IA(LOC+NRCW)
      NBLOCK = IA(IPTR)
      CALL KAXRD(NBLOCK,IB,LBLK)
*
*.....COPY ITS CONTENTS INTO THE ROOT BLOCK
      CALL UCOPY( IB(IB(6)), IA(IA(6)), IB(7)-IB(6) )
      IA(7) = IA(6) + ( IB(7) - IB(6) )
*
*.....DELETE THE REDUNDANT LEVEL
      IA(5) = IA(5) - 1
      CALL KAXWRT(IA(1),IA,IA(7)-1)
      CALL KAXFBK(IB(1))
*
      END
