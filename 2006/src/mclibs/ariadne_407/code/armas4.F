*
* $Id: armas4.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: armas4.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: armas4.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      REAL FUNCTION ARMAS4(I1,I2,I3,I4)
 
C...ARiadne function invariant MASs of 4 partons
 
C...Returns the invariant mass^2 of partons I1, I2, I3 and I4
 
      DIMENSION I(4)
 
 
      I(1)=I1
      I(2)=I2
      I(3)=I3
      I(4)=I4
 
      ARMAS4=ARMASS(4,I)
 
      RETURN
 
C**** END OF ARMAS4 ****************************************************
      END
