*
* $Id: frariad.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frariad.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C************************END FRTESTG ************************************
 
C*********************** SUBROUTINE FRARIAD ***********************
 
      SUBROUTINE FRARIAD
 
C..Fritiof interface to Ariadne_4.02r.  LUJETS entries from IOP(17) to N
C..are copied to Ariadne event record ARJETX, and after emission is done
C..partons are copied back onto LUJETS.
 
 
      PARAMETER (KSZJ=4000,KSZ1=20)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/ARJETX/NO,KO(300,5),PO(300,5),VO(300,5)
      SAVE /FRINTN0/,/LUJETS/,/ARJETX/
 
      NO=0
      DO 100 I=IOP(17),N
      NO=NO+1
      DO 100 LO=1,5
      KO(NO,LO) = K(I,LO)
100   PO(NO,LO) = P(I,LO)
 
      CALL AREXEC
 
      N=IOP(17)-1
      DO 200 IO=1,NO
      IF(KO(IO,1).GE.11) GOTO 200
      N=N+1
      DO 250 LO=1,5
      K(N,LO) = KO(IO,LO)
250   P(N,LO) = PO(IO,LO)
200   CONTINUE
 
      RETURN
      END
