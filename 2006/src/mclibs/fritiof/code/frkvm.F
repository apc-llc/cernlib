*
* $Id: frkvm.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frkvm.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C-------------------------------------------------------------------
 
      FUNCTION FRKVM(W,AM1,AM2)
 
C..FRKVM=3-MOMENTUM K^2 IN A CMS FRAME OF TWO PARTICLES WITH MASS AM1 & AM2:
 
      FRKVM = ((W**2-AM1-AM2)**2-4.*AM1*AM2)/(4.*W**2)
      RETURN
      END
