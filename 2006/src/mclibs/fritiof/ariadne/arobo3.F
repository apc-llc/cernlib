*
* $Id: arobo3.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arobo3.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arobo3.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      SUBROUTINE AROBO3(THE,PHI,DBEX,DBEY,DBEZ,I1,I2,I3)
 
C...Ariadne subroutine ROtate BOost 3 partons
 
C...Rotates and boosts 3 partons in /ARPART/
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      DIMENSION I(3)
 
 
      I(1)=I1
      I(2)=I2
      I(3)=I3
      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,3,I)
 
      RETURN
 
C**** END OF AROBO3 ****************************************************
      END
