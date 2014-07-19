*
* $Id: hnorma.F,v 1.1.1.1 1996/01/16 17:07:44 mclareni Exp $
*
* $Log: hnorma.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:44  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 08/05/89  12.19.12  by  Rene Brun
*-- Author :
      SUBROUTINE HNORMA(ID2,FACTOR)
*.==========>
*.           store normalisation factor for contents
*.           normalisation will be made only at printing
*.           the original contents are not mofified
*..=========> ( R.Brun )
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
*.___________________________________________
      IRET=3
      FACT=FLOARG(FACTOR)
*
  10  CALL HLOOP(ID2,'HNORMA',IRET)
      IF(IRET.EQ.0)GO TO 99
*
      CALL HDCOFL
      IF(I1.EQ.0)GO TO 30
      IF(FACT.GT.0.)THEN
         CALL HSIFLA(18,1)
         Q(LCID+KNORM)=FACT
      ELSE
         CALL HSIFLA(18,0)
         Q(LCID+KNORM)=0.
      ENDIF
*
  30  IRET=2
      GO TO 10
*
  99  RETURN
      END
