*
* $Id: d103f1.F,v 1.1.1.1 1996/02/15 17:48:39 mclareni Exp $
*
* $Log: d103f1.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:39  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
      FUNCTION D103F1(X)
      DOUBLE PRECISION A2,B2,P2,Q2
      COMMON /D103CM/DMY,NFN,A2,B2,P2,Q2
C
C  INTEGRAND FOR SUBPROGRAM GAUSS.
C
C  START.
C
       A=A2
       B=B2
       P=P2
       Q=Q2
C
C  SHARP PEAK.
      IF( NFN.EQ.1 ) D103F1 = P*EXP( MAX( -(P*(X-Q))**2, -100. ) )
C
C  NON-POLYONOMIAL END-POINTS.
      IF( NFN.EQ.2 ) D103F1= 0.5*P*SQRT( (B-A)**2 - (2.*X-B-A)**2 )
C
C  OSCILLATORY FUNCTION.
      IF( NFN.EQ.3 .OR. NFN.EQ.4 ) D103F1 = P*(SIN(X)**2)*COS(Q*X)
      IF( NFN.EQ.4 ) D103F1 = D103F1 + P
C
C  ERROR CONDITION (SINGULARITY).
      IF ( NFN.EQ.5) D103F1 = 1./X**2
C
      RETURN
      END
#endif
