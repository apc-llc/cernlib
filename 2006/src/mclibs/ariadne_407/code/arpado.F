*
* $Id: arpado.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arpado.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arpado.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARPADO(IP,NP,IPV)
 
C...ARiadne subroutine Pointer ADd to Ordered list
 
C...Add pointer IP to ordered vector IPV(NP)
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      DIMENSION IPV(NP+1)
 
      IPV(NP+1)=0
      DO 100 I=1,NP
        IF (IPV(I).EQ.IP) RETURN
        IF (IP.LT.IPV(I)) THEN
          CALL ARPINS(IP,I,NP,IPV)
          RETURN
        ENDIF
 100  CONTINUE
 
      CALL ARPINS(IP,NP+1,NP,IPV)
 
      RETURN
 
C**** END OF ARPADO ****************************************************
      END
