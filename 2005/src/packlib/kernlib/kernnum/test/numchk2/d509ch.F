*
* $Id: d509ch.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: d509ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
      SUBROUTINE D509CH(NREP,OK)
      EXTERNAL D509F
#include "kernnumt/sysdat.inc"
      LOGICAL OK
      OK=.TRUE.
      DO 10 KREP = 1, NREP
         MAXFUN=100
         X=2.
         EPSI=0.01
         STEP=0.1
         A=0.
         B=2.
         CALL MINVAR(X,Y,R,EPSI,STEP,MAXFUN,A,B,D509F)
         D=ABS(X-1.)+ABS(Y-1.)
         OK=D.LE.0.1 .AND. OK
         IF(.NOT.OK) WRITE(*,100)
  10     CONTINUE
      IF(      ERPRNT .AND.       ERSTOP) WRITE(*,101)
      IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,102)
      IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,103)
      MAXFUN  =  2
      CALL MINVAR(X,Y,R,EPSI,STEP,MAXFUN,A,B,D509F)
      RETURN
  100 FORMAT(24H D509CH ARITHMETIC ERROR,4H X= ,G30.15,4H Y= ,G30.15)
  101 FORMAT(/' ONE ERROR AND ABEND MESSAGE SHOULD NOW FOLLOW ...')
  102 FORMAT(/' ONE ERROR MESSAGE SHOULD NOW FOLLOW ...')
  103 FORMAT(/' ONE ABEND MESSAGE SHOULD NOW FOLLOW ...')
      END
