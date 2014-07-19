*
* $Id: frppcol.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: frppcol.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C********************************* END FRANGAN ****************************
 
C********************************* FRPPCOL *****************************
 
          SUBROUTINE FRPPCOL
 
C --- this routine takes care of hadron-hadron collisions
 
      PARAMETER (KSZ1=20,KSZ2=300)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      SAVE /FRINTN0/,/FRINTN3/
 
      IOP(9)=1
      IOP(10)=1
      IOP(2)=1
      NUC(1,1)=1
      NUC(2,1)=1
 
      RETURN
      END
