*
* $Id: iwd16ibm.F,v 1.1.1.1 1996/03/08 15:21:51 mclareni Exp $
*
* $Log: iwd16ibm.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:51  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_ND100B32)
      FUNCTION IWD16(M,N)
C--- FUNCTION RETURNS 16 BIT WORD NO. N OUT OF
C--- 32 BIT WORD ARRAY M.
#if defined(CERNLIB_NORD)
      INTEGER*4 IWD16,N,IOUT
#endif
      INTEGER*2 M(2),I16O(2)
      EQUIVALENCE (IOUT,I16O(1))
      I16O(1)=0
      I16O(2)=M(N)
      IWD16=IOUT
      RETURN
      END
#endif
