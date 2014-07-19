*
* $Id: hfcnv1.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hfcnv1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/09 27/02/94  15.28.56  by  Rene Brun
*-- Author :    Rene Brun   27/02/94
      SUBROUTINE HFCNV1 (N,X,Y,EY,NPAR,IFLAG,UFCN)
*.==========>
*.       Computes Minuit function (vector case with user fitting model)
*..=========> ( R.Brun )
#include "hbook/hcfits.inc"
      DIMENSION X(1),Y(1),EY(1)
      EXTERNAL UFCN
*.___________________________________________
*
      CALL UFCN(N,X,Y,EY,NPAR,IFLAG,NPFITS)
*
      END
