*
* $Id: frangur.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frangur.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END IFRKFC **************************
 
 
C********************************* FRANGUR ******************************
 
      SUBROUTINE FRANGUR(L,J)
 
C-----------------------------------------------------------------------
C     ADD THE DIFFRACTIVE PARTICLES TO THE EVENT RECORD
C-----------------------------------------------------------------------
 
      PARAMETER (KSZJ=4000,KSZ1=20,KSZ2=300)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      SAVE /FRPARA1/,/FRINTN1/,/FRINTN3/,/LUJETS/
 
      N=N+1
      K(N,1)=1
      K(N,2)=IDN(L,J)
      K(N,3)=0
      K(N,4)=0
      K(N,5)=0
      P(N,1)=PPSY(L,J,1)
      P(N,2)=PPSY(L,J,2)
      P(N,3)=(PPSY(L,J,4)-PPSY(L,J,3))/2.
      P(N,4)=(PPSY(L,J,4)+PPSY(L,J,3))/2.
      P(N,5)=ULMASS(IDN(L,J))
 
      RETURN
      END
