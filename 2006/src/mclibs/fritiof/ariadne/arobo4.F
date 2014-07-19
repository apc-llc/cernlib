*
* $Id: arobo4.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arobo4.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arobo4.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      SUBROUTINE AROBO4(THE,PHI,DBEX,DBEY,DBEZ,I1,I2,I3,I4)
 
C...Ariadne subroutine ROtate BOost 4 partons
 
C...Rotates and boosts 4 partons in /ARPART/
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      DIMENSION I(4)
 
 
      I(1)=I1
      I(2)=I2
      I(3)=I3
      I(4)=I4
      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,4,I)
 
      RETURN
 
C**** END OF AROBO4 ****************************************************
      END
