*
* $Id: hbcd.F,v 1.1.1.1 1996/01/16 17:07:31 mclareni Exp $
*
* $Log: hbcd.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:31  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.45  by  Rene Brun
*-- Author :
      SUBROUTINE HBCD(Y,N,IA)
*.==========>
*.           converts real Y in a BCD array IA with N characters
*.           in format F8.3.
*.           if Y.ge.10000  it is converted in an integer BCD array
*..=========> ( R.Brun )
      DIMENSION IA(2)
      IF(Y.LE.10000.)THEN
         CALL HBCDF(Y,N,IA)
      ELSE
         I=Y
         CALL HBCDI(I,9,IA)
      ENDIF
      END
