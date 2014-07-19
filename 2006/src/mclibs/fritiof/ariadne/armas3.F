*
* $Id: armas3.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: armas3.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: armas3.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      REAL FUNCTION ARMAS3(I1,I2,I3)
 
C...ARiadne function invariant MASs of 3 partons
 
C...Returns the invariant mass^2 of partons I1, I2 and I3
 
      DIMENSION I(3)
 
 
      I(1)=I1
      I(2)=I2
      I(3)=I3
 
      ARMAS3=ARMASS(3,I)
 
      RETURN
 
C**** END OF ARMAS3 ****************************************************
      END
