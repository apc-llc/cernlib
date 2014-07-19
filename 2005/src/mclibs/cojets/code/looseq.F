*
* $Id: looseq.F,v 1.1.1.1 1996/01/11 14:14:40 mclareni Exp $
*
* $Log: looseq.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:40  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE LOOSEQ(IQ,N)
C     ***********************
C-- CARRIES ON DECAY OF QUANTUM STORED IN PARQUA(IQ,7)
C-- STABLE CASCADE HADRONS ARE STORED IN P( , ) AND K( , )
C-- N IS LENGTH OF CASCADE
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/decpar.inc"
#include "cojets/edpar.inc"
#include "cojets/event.inc"
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
#include "cojets/maxn.inc"
#include "cojets/par.inc"
#include "cojets/parqua.inc"
C
      EPS=1.E-7
C
      IFLBEG=PARQUA(IQ,6)
      EBEG=PARQUA(IQ,4)
      PP=SQRT(PARQUA(IQ,1)**2+PARQUA(IQ,2)**2+PARQUA(IQ,3)**2)
      THETA=ACOSX(PARQUA(IQ,3)/PP)
      PTQSQ=PARQUA(IQ,1)**2+PARQUA(IQ,2)**2
      IF(PTQSQ.LT.EPS) GO TO 20
      PTQ=SQRT(PTQSQ)
      PARQU1=PARQUA(IQ,1)
      PARQU2=PARQUA(IQ,2)
      PHI=ATAN2X(PARQU2,PARQU1)
      GO TO 21
20    PHI=0.
      PTQ=0.
21    CONTINUE
C
      CALL JETGEN(N)
      IF(N.EQ.0) RETURN
      CALL EDITP(N)
C
      RETURN
      END
