*
* $Id: e401bd.F,v 1.1.1.1 1996/04/01 15:03:15 mclareni Exp $
*
* $Log: e401bd.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:15  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      DOUBLE PRECISION FUNCTION E401BD(A,B)
C
C     CERN PROGRAM LIBRARY NO. E401
C
C     BASIC VERSION OF AUGUST 1970.
C
C     PURPOSE = COMPUTATION OF BINOMIAL COEFFICIENTS
C
C     PARAMETERS
C                                                      ( A )
C     A , B   = USED TO FORM THE BINOMIAL COEFFICIENTS (   ).
C                                                      ( B )
C
      DOUBLE PRECISION A,B
C
      IA=INT(SNGL(A))
      IB=INT(SNGL(B))
      IF((IB.NE.0).AND.(IB.NE.1).AND.(IB.NE.IA).AND.(IB.NE.(IA-1)))
     1GOTO1
      E401BD=A
      IF((IB.EQ.0).OR.(IB.EQ.IA))E401BD=1.D0
      RETURN
C
    1 I1=1
      I2=1
      IBA=IA-IB
      LIM=IB
      IF(IBA.LT.IB)LIM=IBA
         DO 2 N=1,LIM
       I1=I1*(IA-(N-1))
    2 I2=I2*(LIM-(N-1))
      E401BD=DBLE(I1)/DBLE(I2)
      RETURN
C
      END
