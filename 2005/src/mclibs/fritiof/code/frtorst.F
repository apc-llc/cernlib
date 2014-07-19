*
* $Id: frtorst.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frtorst.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRHELGE **************************
 
 
 
C*************************************************************************
C      This is the routine package that sets up strings for Ariadne      *
C*************************************************************************
 
C********************************* FRTORST ******************************
 
      SUBROUTINE FRTORST(L,J)
 
C------------------------------------------------------------------
C Purpose: to set parton codes and momenta before entering ARIADNE and
C JETSET, and to take care of the diffractive hadrons. Fills common block
C LUJETS. J is the nucleon label in nuclus. L=1 for projectile and =2
C for the target.
C From here FRANGUR are called that handles the diffractive particles,
C FRATLEO that sets parton momenta and calls ARIADNE.
C------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (D)
      PARAMETER (KSZ1=20,KSZ2=300)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      SAVE /FRINTN0/,/FRINTN1/,/FRPARA1/
 
        IF(IOP(15).EQ.0.AND.PPSY(L,J,5).LT.AOP(10+L)) THEN
          CALL FRANGUR(L,J)
        ELSE
 
        CALL FRATLEO(L,J)
 
        ENDIF
 
      RETURN
      END
