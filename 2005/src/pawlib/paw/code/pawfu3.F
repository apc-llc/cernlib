*
* $Id: pawfu3.F,v 1.2 1996/03/26 16:46:13 couet Exp $
*
* $Log: pawfu3.F,v $
* Revision 1.2  1996/03/26 16:46:13  couet
* The call to comis interface function CSR... is now done via a local
* copy to a vector.
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/17 16/11/95  09.41.26  by  O.Couet
*-- Author :    O.Couet   24/09/90
      FUNCTION PAWFU3(X,Y,Z)
*
*  Function called by PAFUNC for 3D functions drawing
*
      DIMENSION XYZ(3)
#if defined(CERNLIB_COMIS)
#include "paw/pawcfu.inc"
      XYZ(1) = X
      XYZ(2) = Y
      XYZ(3) = Z
      PAWFU3=CSR3FN(JADF,XYZ(1),XYZ(2),XYZ(3))
#endif
#if !defined(CERNLIB_COMIS)
      PAWFU3=0.
#endif
      END
