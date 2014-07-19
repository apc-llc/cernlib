*
* $Id: d209sc.F,v 1.1.1.1 1996/02/15 17:48:40 mclareni Exp $
*
* $Log: d209sc.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:40  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
      SUBROUTINE D209SC
      DIMENSION ADAT(4)
      DOUBLE PRECISION DADAT(4),DQDAT(4,4),DN,DS
      DOUBLE PRECISION DA,DQ
      COMMON /D209CM/N,ITRANS,A(4),Q(4,4),DA(4),DQ(4,4)
      DATA DQDAT/+1D0,+1D0,+1D0,+1D0,
     *           +1D0,-1D0,+1D0,-1D0,
     *           +1D0,+1D0,-1D0,-1D0,
     *           +1D0,-1D0,-1D0,+1D0/
      DATA ADAT/ 0.25, 0.6, 0.03, 0.8 /
      DATA DADAT/ 0.25D0, 0.6D0, 0.002D0, 0.5D0 /
C
C  SETS SCALING CONSTANTS IN ARRAY A (REAL) AND DA (DOUBLE PRECISION).
C  SETS AN ORTHOGONAL MATRIX IN ARRAY Q (REAL) AND DQ (DOUBLE PRE-
C  CISION).
C  CALLS ... CERN PACKAGES F002 AND F003.
C
C  START.
      CALL RVCPY(4,ADAT,ADAT(2),A,A(2))
      CALL DVCPY(4,DADAT,DADAT(2),DA,DA(2))
      DN=N
      DS=1D0/DSQRT(DN)
      CALL DMSCL(4,4,DS,DQDAT,DQDAT(1,2),DQDAT(2,1),DQ,DQ(1,2),DQ(2,1))
      DO 2 I=1,N
         DO 1 J=1,N
            Q(I,J)=DQ(I,J)
    1    CONTINUE
    2 CONTINUE
      RETURN
      END
#endif
