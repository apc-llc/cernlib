*
* $Id: arobo5.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arobo5.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arobo5.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      SUBROUTINE AROBO5(THE,PHI,DBEX,DBEY,DBEZ,I1,I2,I3,I4,I5)
 
C...Ariadne subroutine ROtate BOost 5 partons
 
C...Rotates and boosts 5 partons in /ARPART/
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      DIMENSION I(5)
 
 
      I(1)=I1
      I(2)=I2
      I(3)=I3
      I(4)=I4
      I(5)=I5
      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,5,I)
 
      RETURN
 
C**** END OF AROBO5 ****************************************************
      END
