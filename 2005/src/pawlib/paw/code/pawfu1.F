*
* $Id: pawfu1.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawfu1.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/17 17/11/95  12.12.09  by  O.Couet
*-- Author :    O.Couet   17/11/95
      FUNCTION PAWFU1(X)
*
*  Function called by PAFUNC for 1D functions drawing
*
#if defined(CERNLIB_COMIS)
#include "paw/pawcfu.inc"
      PAWFU1=CSR1FN(JADF,X)
#endif
#if !defined(CERNLIB_COMIS)
      PAWFU1=0.
#endif
*
      END
