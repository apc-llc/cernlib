*
* $Id: frchexg.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frchexg.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRPLIMT **************************
 
C********************************* FRCHEXG ***************************
 
      SUBROUTINE FRCHEXG(*,I)
 
C.....To administrate the preliminary checking and testing of RPS
C.....This routine will manages the final gluon emission if there is only
C.....one collision, else the gluon emission will be managed in FRINGEB.
C...  IOP(15)>=2 signals hard partons are drawned.
 
      IMPLICIT DOUBLE PRECISION (D)
      PARAMETER (KSZJ=4000,KSZ1=20,KSZ2=300)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      COMMON/FRJETS/NJ,KJ(100,5),PJ(100,5)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      SAVE /FRINTN0/,/FRPARA1/,/FRINTN3/,/FRJETS/,/LUJETS/
 
       IF(IOP(15).EQ.0.AND.NJ.GT.0) IOP(15)=1
 
       CALL FRTORST(1,NUC(1,I))
       IF(IOP(15).NE.2) CALL FRTORST(2,NUC(2,I))
 
       IF(IOP(15).GE.2) THEN
 
       CALL FRFILHD(I,-1,KFEL)
 
       RETURN 1
 
       ENDIF
 
       RETURN
       END
