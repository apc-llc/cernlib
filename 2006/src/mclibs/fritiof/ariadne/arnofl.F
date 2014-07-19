*
* $Id: arnofl.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: arnofl.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arnofl.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      REAL FUNCTION ARNOFL(W,MNOFL)
 
C...ARiadne function Number Of FLavours
 
C...Returns the number of flavourspossible at energy W
 
 
      COMMON /ARDAT2/ PQMAS(10)
      SAVE /ARDAT2/
 
 
      ARNOFL=0.0
      DO 100 I=1,MNOFL
        IF(W.LT.2.0*PQMAS(I)) RETURN
        ARNOFL=FLOAT(I)
 100  CONTINUE
 
      RETURN
 
C**** END OF ARNOFL ****************************************************
      END
