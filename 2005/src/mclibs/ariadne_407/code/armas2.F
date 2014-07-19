*
* $Id: armas2.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: armas2.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: armas2.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      REAL FUNCTION ARMAS2(I1,I2)
 
C...ARiadne function invariant MASs of 2 partons
 
C...Returns the invariant mass^2 of partons I1 and I2
 
      DIMENSION I(2)
 
 
      I(1)=I1
      I(2)=I2
 
      ARMAS2=ARMASS(2,I)
 
      RETURN
 
C**** END OF ARMAS2 ****************************************************
      END
