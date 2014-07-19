*
* $Id: arpadd.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arpadd.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arpadd.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARPADD(IP,NP,IPV)
 
C...ARiadne subroutine Pointer ADD
 
C...Add pointer IP to vector IPV(NP)
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      DIMENSION IPV(NP+1)
 
      IF (IP.GT.0) GOTO 900
      DO 100 I=1,NP
        IF (IPV(I).EQ.ABS(IP)) RETURN
 100  CONTINUE
 
 900  NP=NP+1
      IPV(NP)=ABS(IP)
 
      RETURN
 
C**** END OF ARPADD ****************************************************
      END
