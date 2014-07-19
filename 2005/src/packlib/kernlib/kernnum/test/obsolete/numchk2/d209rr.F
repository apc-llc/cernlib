*
* $Id: d209rr.F,v 1.1.1.1 1996/02/15 17:48:40 mclareni Exp $
*
* $Log: d209rr.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:40  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
      SUBROUTINE D209RR(X,Y,F)
      DIMENSION Y(4),F(4),U(4)
      DOUBLE PRECISION DA,DQ
      COMMON /D209CM/N,ITRANS,A(4),Q(4,4),DA(4),DQ(4,4)
C
C  SUBROUTINE WHICH SETS RIGHT-HAND SIDES (DERIVATIVES) FOR RKSTP AND
C  DRKSTP.
C  CALLS ... CERN PACKAGES F002 AND F003.
C
C  START.
      IF(ITRANS.EQ.1) CALL RVCPY(N,Y,Y(2),U,U(2))
      IF(ITRANS.EQ.2) CALL RMMPY(N,N,Q,Q(1,2),Q(2,1),Y,Y(2),U,U(2))
      DO 1 J=1,N
         AJ=A(J)
         IF(J.EQ.1)  U(J)=AJ*EXP(AJ*X)*SQRT( ABS(1.-U(J)**2) )
         IF(J.EQ.2)  U(J)=AJ/COS(AJ*X)**2
         IF(J.EQ.3)  U(J)=2.*AJ*SQRT(U(J))/(1.+AJ*X)
         IF(J.EQ.4)  U(J)=AJ*U(J)*COS(AJ*X)
    1 CONTINUE
      IF(ITRANS.EQ.1) CALL RVCPY(N,U,U(2),F,F(2))
      IF(ITRANS.EQ.2) CALL RMMPY(N,N,Q,Q(1,2),Q(2,1),U,U(2),F,F(2))
      RETURN
      END
#endif
