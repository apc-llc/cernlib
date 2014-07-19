*
* $Id: e106ch.F,v 1.1.1.1 1996/02/15 17:48:43 mclareni Exp $
*
* $Log: e106ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:43  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE E106CH(NREP,IDIM,FD,ID,OK)
          INTEGER   ID(IDIM)
          REAL      FD(IDIM)
          LOGICAL   OK
#include "kernnumt/sysdat.inc"
          DATA      EPS       / 0.001 /
          IF(IDIM .LT. 20)  GOTO 900
          OK  =  .TRUE.
          N   =  0
          DO 100    JREP  =  1, NREP
  10         N  =  N+1
             IF(N .GT. 18)   N  =  N+9
             IF(N .GT. 100)  N  =  N+90
             IF(N .GT. IDIM) N  =  1
  11         STEP  =  10. * RANF()
             IF(STEP .LE. EPS)  GOTO 11
             FORG  =  -0.5 * STEP * FLOAT(N)
             DO 20  J  =  1, N
                FD(J)  =  FORG + FLOAT(J-1)*STEP
                ID(J)  =  INT(FD(J))
  20            CONTINUE
             DO 30  J  =  1, N
                FEL  =  FD(J)
                FEP  =  FEL + EPS
                FEM  =  FEL - EPS
                LFE  =  LOCATF(FD(1),N,FEL)
                LFP  =  LOCATF(FD(1),N,FEP)
                LFM  =  LOCATF(FD(1),N,FEM)
                IF(LFE-J .NE. 0  .OR.
     +             LFP+J .NE. 0  .OR.
     +             LFM+J-1 .NE. 0)  GOTO 60
                IEL  =  ID(J)
                IEP  =  IEL + 1
                IEM  =  IEL - 1
                LIE  =  LOCATI(ID(1),N,IEL)
                LIP  =  LOCATI(ID(1),N,IEP)
                LIM  =  LOCATI(ID(1),N,IEM)
                IF(LIE .NE. J  .AND. ID(LIE) .EQ. IEL) LIE = J
                IF(J .NE. N  .AND.  IEP .GE. ID(J+1))  LIP = -J
                IF(J .NE. 1  .AND.  IEM .LE. ID(J-1))  LIM = 1-J
                IF(LIE-J .NE. 0  .OR.
     +             LIP+J .NE. 0  .OR.
     +             LIM+J-1 .NE. 0)  GOTO 70
  30            CONTINUE
 100         CONTINUE
          RETURN
  60      WRITE(*,1001) NREP,N,J,FD(J),FEM,FEL,FEP,LFM,LFE,LFP
          OK  =  .FALSE.
          RETURN
  70      WRITE(*,1002) NREP,N,J,ID(J),IEM,IEL,IEP,LIM,LIE,LIP
          OK  =  .FALSE.
          RETURN
 900      WRITE(*,1000) IDIM
          RETURN
1000      FORMAT(34H ??? MORE SPACE NEEDED THAN IDIM =,I3)
1001      FORMAT(20H ??? ERROR IN LOCATF, / 3I10,4F15.6,3I10)
1002      FORMAT(20H ??? ERROR IN LOCATI, / 10I8)
          END
