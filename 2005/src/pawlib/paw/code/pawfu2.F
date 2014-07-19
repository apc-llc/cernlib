*
* $Id: pawfu2.F,v 1.2 1996/03/26 16:46:11 couet Exp $
*
* $Log: pawfu2.F,v $
* Revision 1.2  1996/03/26 16:46:11  couet
* The call to comis interface function CSR... is now done via a local
* copy to a vector.
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/17 17/11/95  12.12.09  by  O.Couet
*-- Author :    O.Couet   17/11/95
      FUNCTION PAWFU2(X,Y)
*
*  Function called by PAFUNC for 2D functions drawing
*
      DIMENSION XY(2)
#if defined(CERNLIB_COMIS)
#include "paw/pawcfu.inc"
      XY(1) = X
      XY(2) = Y
      PAWFU2=CSR2FN(JADF,XY(1),XY(2))
#endif
#if !defined(CERNLIB_COMIS)
      PAWFU2=0.
#endif
*
      END
