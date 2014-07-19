*
* $Id: arpins.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arpins.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arpins.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARPINS(IP,INP,NP,IPV)
 
C...ARiadne subroutine Pointer INSert
 
C...Insert pointer IP to vector IPV(NP) in position I
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      DIMENSION IPV(NP+1)
 
      DO 100 I=NP,INP,-1
        IPV(I+1)=IPV(I)
 100  CONTINUE
      IPV(INP)=IP
 
      NP=MAX(NP+1,INP)
 
      RETURN
 
C**** END OF ARPINS ****************************************************
      END
