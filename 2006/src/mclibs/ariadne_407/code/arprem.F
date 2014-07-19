*
* $Id: arprem.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arprem.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arprem.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARPREM(IP,NP,IPV)
 
C...ARiadne subroutine Pointer REMove
 
C...Remove pointer IP from vector IPV(NP) and purge the vector.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      DIMENSION IPV(NP)
 
      NFOUND=0
      DO 100 I=1,NP
        IF (NFOUND.GT.0) IPV(I-NFOUND)=IPV(I)
        IF (IPV(I).EQ.IP) NFOUND=NFOUND+1
 100  CONTINUE
 
      NP=NP-NFOUND
 
      RETURN
 
C**** END OF ARPREM ****************************************************
      END
