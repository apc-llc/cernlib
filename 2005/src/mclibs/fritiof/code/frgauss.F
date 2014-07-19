*
* $Id: frgauss.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frgauss.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRHPLIS ***************************
 
 
 
C*************************************************************************
C**                                                                     **
C**   This is the package for auxililary subroutines                    **
C**                                                                     **
C*************************************************************************
 
C********************************* FRGAUSS ****************************
 
      SUBROUTINE FRGAUSS(P2,V,PMAX)
 
C.... TO RETURN A VALUE P2 WHICH HAS A MAXIMUM SET BY PMAX, AND A
C.... 2-D GAUSSIAN DISTRIBUTION WITH WIDTH V, i.e., e^(-P2/V)dP2, 0<P2<PMAX.
C.... set PMAX < 0 if PMAX should be infinity.
 
      P2 = 0
      IF(V.LE.1.E-8) RETURN
 
      IF(PMAX.LT.0) THEN
      A = 1.
      ELSEIF(PMAX.LT.1.E-9) THEN
      RETURN
      ELSE
      A = 1. - FRREX(-PMAX/V)
      ENDIF
 
10    P2 = -V* LOG(MAX(1.E-20,1. - A*RLU(0)))
      IF(P2.LT.0.) GOTO 10
 
      RETURN
      END
