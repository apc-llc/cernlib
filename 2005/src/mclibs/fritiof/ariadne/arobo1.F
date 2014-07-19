*
* $Id: arobo1.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arobo1.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arobo1.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      SUBROUTINE AROBO1(THE,PHI,DBEX,DBEY,DBEZ,I1)
 
C...Ariadne subroutine ROtate BOost 1 parton
 
C...Rotates and boosts 1 parton in /ARPART/
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      DIMENSION I(1)
 
 
      I(1)=I1
      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,1,I)
 
      RETURN
 
C**** END OF AROBO1 ****************************************************
      END
