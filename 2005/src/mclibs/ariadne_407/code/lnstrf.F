*
* $Id: lnstrf.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
*
* $Log: lnstrf.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:06  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
 
      SUBROUTINE LNSTRF(X,XQ2,XPQ)
 
C...ariadne dummy routine LNSTRF
 
C...Produce an error message if Lepto structure functions has not
C...been linked.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      REAL XPQ(-6:6)
 
 
      CALL ARERRM('LNSTRF',24,0)
 
      RETURN
 
C**** END OF LNSTRF ****************************************************
      END
