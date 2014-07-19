*
* $Id: arrobo.F,v 1.1.1.1 1996/03/08 16:51:04 mclareni Exp $
*
* $Log: arrobo.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:04  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arrobo.F,v 1.1.1.1 1996/03/08 16:51:04 mclareni Exp $
 
      SUBROUTINE ARROBO(THE,PHI,DBEX,DBEY,DBEZ,NI,I)
 
C...ARiadne subroutine ROtate and BOost
 
C...Rotates and boost NI particles in /ARPART/
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      DIMENSION I(NI)
 
 
      CALL ARDBRB(DBLE(THE),DBLE(PHI),DBEX,DBEY,DBEZ,NI,I)
 
      RETURN
 
C**** END OF ARROBO ****************************************************
      END
