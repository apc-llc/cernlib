*
* $Id: arobo2.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arobo2.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arobo2.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      SUBROUTINE AROBO2(THE,PHI,DBEX,DBEY,DBEZ,I1,I2)
 
C...Ariadne subroutine ROtate BOost 2 partons
 
C...Rotates and boosts 2 partons in /ARPART/
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      DIMENSION I(2)
 
 
      I(1)=I1
      I(2)=I2
      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,2,I)
 
      RETURN
 
C**** END OF AROBO2 ****************************************************
      END
