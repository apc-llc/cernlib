*
* $Id: hexp.F,v 1.1.1.1 1996/01/16 17:08:12 mclareni Exp $
*
* $Log: hexp.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:12  mclareni
* First import
*
*
#include "sys/CERNLIB_machine.h"
#include "_hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.47  by  Rene Brun
*-- Author :
      FUNCTION HEXP (ARGUM)
*.==========>
*.           CALCULATES VALUE OF THE FUNCTION
*.           F(X)=EXP(A+B*X)
*.           AND ITS DERIVATIVES
*..=========> ( R.Brun )
#include "hcfit3.inc"
#include "hcbook.inc"
*.___________________________________________
      TEMP=EXP(Q(IPARAM)+Q(IPARAM+1)*ARGUM)
      Q(IDERIV)=TEMP
      Q(IDERIV+1)=TEMP*ARGUM
      HEXP=TEMP
      END
