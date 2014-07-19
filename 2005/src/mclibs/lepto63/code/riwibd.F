
C **********************************************************************

      SUBROUTINE RIWIBD
C   BLOCK DATA SUBSTITUTE from RIWIAD
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/STORE/XA(11),XB(11),XC(11),XD(11),MA(11),MB(11),MC(11)
      COMMON/STORE1/R(10000),LR
      COMMON/OPTION/IPRRIW,ICONV,IRESET
      COMMON/RANDOM/NSHOTS
      COMMON/INTERN/FACTOR,ALFA,BETA,GAMMA,DELTA,LEVEL,NMIN
      COMMON /LPFLAG/ LST3
      DATA INIT/0/
      IF(INIT.EQ.1) RETURN
      INIT=1
      MA(1)=0
      LR=10000
      ICONV=1
      IRESET=0
      NSHOTS=2
      FACTOR=1.65
      LEVEL=90
      ALFA=0.3
      BETA=0.3
      GAMMA=0.3
      DELTA=.7
      NMIN=2
C...Print flag to be changed here.
      IPRRIW=0
      IF(LST3.GE.4) WRITE(6,1000) IPRRIW
      RETURN
 1000 FORMAT(5X,'RIWIAD print flag changed: IPRRIW =',I5)
      END
