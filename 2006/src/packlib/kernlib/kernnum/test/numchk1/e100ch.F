*
* $Id: e100ch.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: e100ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
      SUBROUTINE E100CH(NP,P,NQ,Q,NREP,OK)
      DIMENSION P(NP),Q(NQ)
#include "kernnumt/sysdat.inc"
      LOGICAL OK,OKT
      OK=.TRUE.
C
C      COMPUTE INTERPOLATED VALUE OF SIN(PI/6)
C
      X=4.*ATAN(1.)/6.
      R=SIN(X)
     C
C      CHOOSE NREP X-VALUES AND COMPUTE FUNCTION VALUES
C
      DO 1 I=1,NREP
           Q(I)=FLOAT(I)/7.
           P(I)=SIN(Q(I))
 1    CONTINUE
C
C      COMPUTE INTERPOLATION VALUE FOR DIFFERENT DEGREES N
C
      DO 2 N=2,NREP
           CALL POLINT(P,Q,N,X,S)
           D=2.*ABS(S-R)
#if defined(CERNLIB_NUMHIPRE)
           NEXP=MIN0(13,N-1)
#endif
#if defined(CERNLIB_NUMLOPRE)
           NEXP=MIN0(5,N-1)
#endif
           REL=5.*10.**(-NEXP)
           OKT=D .LE.REL
           IF(.NOT. OKT) WRITE(*,100) N,D,REL
           OK=OK .AND. OKT
 2    CONTINUE
      IF(      ERPRNT .AND.       ERSTOP) WRITE(*,101)
      IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,102)
      IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,103)
      N  =  1
      CALL POLINT(P,Q,N,X,S)
      RETURN
  100 FORMAT(/ 25H *** ARITHMETIC ERROR ***, I8,1P,E12.3,1P,E12.3)
  101 FORMAT(/' ONE ERROR AND ABEND MESSAGE SHOULD NOW FOLLOW ...')
  102 FORMAT(/' ONE ERROR MESSAGE SHOULD NOW FOLLOW ...')
  103 FORMAT(/' ONE ABEND MESSAGE SHOULD NOW FOLLOW ...')
      END
