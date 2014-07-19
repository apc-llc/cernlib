*
* $Id: c204ch.F,v 1.1.1.1 1996/02/15 17:48:39 mclareni Exp $
*
* $Log: c204ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:39  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
          SUBROUTINE C204CH(NA,A,ROOT,NREP,OK)
          DIMENSION A(NA),ROOT(NA)
          COMPLEX ROOT,SUM,VSUM
#include "kernnumt/sysdat.inc"
          LOGICAL OK,OKT
#if defined(CERNLIB_IBMRT)
          EXTERNAL CABS
#endif
          OK=.TRUE.
C
C         ROOTS OF TRUNCATED EXPONENTIAL SERIES WITH DEGREE 1,...,NA
C
          REL=RELPR*1.E3
          DO 20     JREP  =  1, NREP
             DO 10  N  =  1, NA
                A(N+1)=1.
                J=N+1
                DO 2 I=1,N
                J=J-1
    2           A(J)=A(J+1)/FLOAT(I)
                FL=FLOAT(N)
                VSUM=CMPLX(FL,0.)
                CALL MULLER(A,N,ROOT)
                SUM=0.
                DO 3 K=1,N
    3           SUM=ROOT(K)+SUM
                D=CABS(SUM+VSUM)
                OKT=D.LE.REL*FL
                IF(.NOT.OKT) WRITE(*,100) N
                OK=OK.AND.OKT
   10           CONTINUE
   20        CONTINUE
          IF(      ERPRNT .AND.       ERSTOP) WRITE(*,101)
          IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,102)
          IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,103)
          N  =  0
          CALL MULLER(A,N,ROOT)
          N  =  NA
          A(1)  =  0.
          CALL MULLER(A,N,ROOT)
          RETURN
  100     FORMAT(36HC204CH ARITHMETIC ERROR AT DEGREE N=, I5)
  101 FORMAT(/' TWO ERROR AND ABEND MESSAGES SHOULD NOW FOLLOW ...')
  102 FORMAT(/' TWO ERROR MESSAGES SHOULD NOW FOLLOW ...')
  103 FORMAT(/' TWO ABEND MESSAGES SHOULD NOW FOLLOW ...')
      END
#endif
