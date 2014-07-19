*
* $Id: iwd16n16.F,v 1.1.1.1 1996/03/08 15:21:58 mclareni Exp $
*
* $Log: iwd16n16.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:58  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_ND100B16)
      FUNCTION IWD16(M,N)
C.+++++++ NORD 10/100 VERSION ++++++++++
C--- FUNCTION RETURNS 16 BIT WORD NO. N OUT OF
C--- 32 BIT WORD ARRAY M.
      INTEGER*2 M(2)
       IWD16=M(N)
      RETURN
      END
#endif
