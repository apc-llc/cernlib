*
* $Id: fzon4.F,v 1.2 1999/06/18 13:29:36 couet Exp $
*
* $Log: fzon4.F,v $
* Revision 1.2  1999/06/18 13:29:36  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.1.1.1  1996/03/06 10:47:13  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
#if defined(CERNLIB_FZFFNAT)
      SUBROUTINE FZON4 (V1,N1,V2,N2,V3,N3,V4,N4)

#include "zebra/mqsys.inc"
#include "zebra/eqlqf.inc"
#include "zebra/fzcx.inc"
*
#if !defined(CERNLIB_AUTODBL)
      INTEGER   V1(N1),V2(N2),V3(N3),V4(N4)
#endif
#if defined(CERNLIB_AUTODBL)
      REAL   V1(N1),V2(N2),V3(N3),V4(N4),RELIDX(2)
      EQUIVALENCE (RELIDX(1),IDX(1))
#endif

      IDX(1) = N1 + N2 + N3 + N4

#if !defined(CERNLIB_AUTODBL)
      WRITE (LUNX) IDX,V1,V2,V3,V4
#endif
#if defined(CERNLIB_AUTODBL)
      WRITE (LUNX) RELIDX,V1,V2,V3,V4
#endif
      IQ(KQSP+LQFX+20) = IQ(KQSP+LQFX+20) + IDX(1) + 3
      IQ(KQSP+LQFX+21) = IQ(KQSP+LQFX+21) + 1
      RETURN
      END
#endif
