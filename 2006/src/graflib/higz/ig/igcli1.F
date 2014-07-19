*
* $Id: igcli1.F,v 1.1.1.1 1996/02/14 13:10:33 mclareni Exp $
*
* $Log: igcli1.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:33  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 24/07/89  16.45.24  by  O.Couet
*-- Author :    O.Couet   24/07/89
      FUNCTION IGCLI1(X,Y,XCL1,XCL2,YCL1,YCL2)
*.===========>
*.
*.   Compute the endpoint codes for IGCLIP.
*.
*..==========> (O.Couet)
      IGCLI1=0
      ICODE=0
      IF(X.LT.XCL1)CALL SBIT1(ICODE,1)
      IF(X.GT.XCL2)CALL SBIT1(ICODE,2)
      IF(Y.LT.YCL1)CALL SBIT1(ICODE,3)
      IF(Y.GT.YCL2)CALL SBIT1(ICODE,4)
      IGCLI1=ICODE
*
      END

