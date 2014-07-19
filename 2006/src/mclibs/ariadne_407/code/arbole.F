*
* $Id: arbole.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arbole.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arbole.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARBOLE(THEL,PHI1,PHI2,DBXL,DBYL,DBZL)
 
C...ARiadne subroutine BOost to hadronic center of mass of LEpto event
 
C...Boost partons to the hadronic CMS of a LEPTO event
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
 
 
      DBXL=0.0D0
      DBYL=0.0D0
      DBZL=0.0D0
      DBEL=0.0D0
      THEL=0.0
      PHI1=0.0
      PHI2=0.0
 
      DO 100 I=5,N
        DBXL=DBXL+DBLE(P(I,1))
        DBYL=DBYL+DBLE(P(I,2))
        DBZL=DBZL+DBLE(P(I,3))
        DBEL=DBEL+DBLE(P(I,4))
 100  CONTINUE
 
      DBXL=DBXL/DBEL
      DBYL=DBYL/DBEL
      DBZL=DBZL/DBEL
 
      CALL LUDBRB(1,N,0.0,0.0,-DBXL,-DBYL,-DBZL)
      PHI1=ULANGL(P(3,1),P(3,2))
      THEL=ULANGL(P(3,3),SQRT(P(3,1)**2+P(3,2)**2))
 
      CALL LUDBRB(1,N,0.0,-PHI1,0.0D0,0.0D0,0.0D0)
      CALL LUDBRB(1,N,-THEL,0.0,0.0D0,0.0D0,0.0D0)
 
      PHI2=ULANGL(P(1,1),P(1,2))
      CALL LUDBRB(1,N,0.0,-PHI2,0.0D0,0.0D0,0.0D0)
 
      RETURN
 
C**** END OF ARBOLE ****************************************************
      END
