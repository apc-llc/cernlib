*
* $Id: iwd16vax.F,v 1.1.1.1 1996/03/08 15:21:49 mclareni Exp $
*
* $Log: iwd16vax.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:49  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_VAX)||defined(CERNLIB_APOLLO)
      FUNCTION IWD16(M,N)
C--- FUNCTION RETURNS 16 BIT WORD NO. N OUT OF
C--- 32 BIT WORD ARRAY M.
      INTEGER*2 M(2)
      IWD16= M(N)
#if defined(CERNLIB_VAX)
      IWD16 = IWD16.AND.'FFFF'X
#endif
#if defined(CERNLIB_APOLLO)
      IWD16 = AND(IWD16,16#FFFF)
#endif
      RETURN
      END
#endif
