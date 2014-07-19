*
* $Id: e208ch.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: e208ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE E208CH(NDIM,X,Y,MDIM,A,C,NREP,OK)
          REAL      X(NDIM), Y(NDIM), A(MDIM), C(MDIM)
          LOGICAL   OK, OKT
#include "kernnumt/sysdat.inc"
          DATA      IDIM      /  20  /
          DATA      MARG      /  400  /
          IRESF(RES)  =  NINT(RES/RELPR)
          OK  =  .TRUE.
          DO 100    IREP  =  1, NREP
             DO 50  M  =  1, MDIM
                DO 10   I  =  1, M
                   C(I)  =  2.*RANF() - 1.
  10               CONTINUE
                N  =  M + INT(FLOAT(NDIM-M+1)*RANF())
                IF(N .GE. 2*M)  GOTO 15
  11               X(1)  =  4.*RANF() - 2.
                   IF(ABS(X(1)) .LT. 1.)  GOTO 11
                   Y(1)  =  C(M)
                   IF(N .EQ. 1)  GOTO 21
                   H  =  -2.05*X(1) / FLOAT(N-1)
                   DO 12    J  =  2, N
                      X(J)  =  X(J-1) + H
                      Y(J)  =  C(M)
  12                  CONTINUE
                   GOTO 21
  15            DO 20   J  =  1, N
                   X(J)  =  2.*RANF() - 1.
                   Y(J)  =  C(M)
  20               CONTINUE
  21            IF(M .EQ. 1)  GOTO 30
                DO 25   I  =  2, M
                   K  =  M - I + 1
                   DO 24  J  =  1, N
                      Y(J)  =  Y(J)*X(J) + C(K)
  24                  CONTINUE
  25               CONTINUE
  30            CALL LSQ(N,X,Y,M,A)
                DO 40   I  =  1, M
                   RES   =  ABS(C(I)-A(I))
                   IREL  =  IRESF(RES)
                   IF(IREL .LE. MARG)  GOTO 40
                      WRITE(*,1000) IREP, M, N,RES,IREL,MARG
                      OK  =  .FALSE.
  40               CONTINUE
                IF(M .NE. 2)  GOTO 50
                X(1)  =  0.
                X(2)  =  X(1)
                CALL LLSQ(2,X,Y,A(1),A(2),IFAIL)
                OKT  =  IFAIL .EQ. -1
                IF(.NOT. OKT)  WRITE(*,1001) IREP, IFAIL
                OK  =  OK .AND. OKT
                N  =  1 - INT(3.*RANF())
                CALL LLSQ(N,X,Y,A(1),A(2),IFAIL)
                OKT  =  IFAIL .EQ. -2
                IF(.NOT. OKT)  WRITE(*,1002) IREP, N, IFAIL
                OK  =  OK .AND. OKT
  50            CONTINUE
 100         CONTINUE
          IF(ERPRNT .AND. ERSTOP)      WRITE(*,1110)
          IF(ERPRNT .AND..NOT. ERSTOP) WRITE(*,1111)
          IF(.NOT. ERPRNT .AND. ERSTOP)WRITE(*,1112)
          M  =  1
          N  =  0
          CALL LSQ(N,X,Y,M,A)
          IF(M .NE. 0)  THEN
             OK  =  .FALSE.
             WRITE(*,1100) M, 'E208.1', '(N.LT.M)'
          ENDIF
          M  =  -1
          N  =  30
          CALL LSQ(N,X,Y,M,A)
          IF(M .NE. 0)  THEN
             OK  =  .FALSE.
             WRITE(*,1100) M, 'E208.1', '(M.LT.1)'
          ENDIF
          M  =  IDIM+1
          N  =  M
          CALL LSQ(N,X,Y,M,A)
          IF(M .NE. 0)  THEN
             OK  =  .FALSE.
             WRITE(*,1100) M, 'E208.1', '(M.LT.IDIM)'
          ENDIF
          M  =  5
          N  =  M
          DO 110    I  =  1, M
             X(I)  =  3.
             Y(I)  =  0.
 110         CONTINUE
          CALL LSQ(N,X,Y,M,A)
          IF(M .NE. 0)  THEN
             OK  =  .FALSE.
             WRITE(*,1100) M, 'E208.2'
          ENDIF
          RETURN
1000      FORMAT(10H E208CH   ,5HIREP=,I3,4H  M=,I2,4H  N=,I3,
     +           6H  RES=,1P,E10.2,7H  IREL=,I6,15H  EXCEEDS MARG=,I4)
1001      FORMAT(10H E208CH   ,5HIREP=,I3,8H  IFAIL=,I3,8H .NE. -1)
1002      FORMAT(10H E208CH   ,5HIREP=,I3,4H  N=,I3,8H  IFAIL=,I3,
     +              8H .NE. -2)
1100      FORMAT( / ' ????? TEST OF LSQ ... M =', I6,
     +           ' ERROR CONDITION ', A6, ' NOT DETECTED. ', A11)
1110     FORMAT(/' FOUR ERROR AND ABEND MESSAGES SHOULD NOW FOLLOW ...')
1111      FORMAT(/' FOUR ERROR MESSAGES SHOULD NOW FOLLOW ...')
1112      FORMAT(/' FOUR ABEND MESSAGES SHOULD NOW FOLLOW ...')
          END
