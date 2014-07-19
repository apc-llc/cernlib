*
* $Id: aripsf.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
*
* $Log: aripsf.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:06  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: aripsf.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
 
      REAL FUNCTION ARIPSF(A,N,X)
 
C...ARiadne function Integrate part of Pomeron Structure Function
 
C...Returns the integral from X to 1 of the function Z**A*(1-Z)**N for
C...A>-1 and N>=0 used in calculation of pomeron part of nucleon
C...structure function
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
 
      FAC=1.0
      SUM=0.0
      NI=N
      AI=A
      ARIPSF=1.0/SQRT(A+1.0)
      IF (N.LT.0.OR.A.LE.-1.0.OR.X.LT.0.0.OR.X.GE.1.0) RETURN
 
 100  IF (NI.EQ.0) THEN
        SUM=SUM+FAC*(1.0-X**(AI+1.0))/(AI+1.0)
        ARIPSF=SUM
        RETURN
      ELSE
        AI=AI+1
        FAC=FAC/AI
        SUM=SUM-FAC*(X**AI)*((1.0-X)**NI)
        FAC=FAC*NI
        NI=NI-1
        GOTO 100
      ENDIF
 
C**** END OF ARIPSF ****************************************************
      END
