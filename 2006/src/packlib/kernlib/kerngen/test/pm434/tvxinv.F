*
* $Id: tvxinv.F,v 1.1.1.1 1996/02/15 17:54:56 mclareni Exp $
*
* $Log: tvxinv.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:56  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TVXINV

#if defined(CERNLIB_TVXINV)
#include "kerngent/mkcde.inc"

C-----------       VXINVB

      CALL NEWGUY ('VXINVB - VXINVC.','TVXINV  ')
      CALL VZERO (A,2000)

      NTN = 200
      NTX = NTN - 2
      JS  = 0

      DO 24  J=1,NTN
      CALL SBYT (JS+3, IB(J), 25,8)
      CALL SBYT (JS+2, IB(J), 17,8)
      CALL SBYT (JS+1, IB(J),  9,8)
      CALL SBYT (JS,   IB(J),  1,8)

      CALL SBYT (JS+3, IA(J),  1,8)
      CALL SBYT (JS+2, IA(J),  9,8)
      CALL SBYT (JS+1, IA(J), 17,8)
      CALL SBYT (JS,   IA(J), 25,8)

      JS = JS + 1
      IF (JS.EQ.254)  JS= 0
   24 CONTINUE

      DO 25  J=NTX+1,NTN
   25 IA(J) = IB(J)
      CALL UCOPY (IA, IA(501), NTN)

      CALL VXINVB (IA,NTX)
      CALL MVERII (1,IA,IB,NTN)

C-----------       VXINVC

      CALL VZERO  (IA,NTX)
      CALL VXINVC (IA(501),IA,NTX)
      CALL MVERII (2,IA,IB,NTN)

C--      TIMING

      IF (ITIMES.EQ.0)       RETURN
      NTIMES = ITIMES*1000
      CALL TIMED (TIMERD)

      DO 72 J= 1,NTIMES
      CALL VXINVB (IA,1000)
   72 CONTINUE
      CALL TIME77 (NTIMES,'VXINVB ',1000,'words')

      CALL TIMED (TIMERD)

      DO 74 J= 1,NTIMES
      CALL VXINVC (IA,IB,1000)
   74 CONTINUE
      CALL TIME77 (NTIMES,'VXINVC ',1000,'words')
#endif
      RETURN
      END
