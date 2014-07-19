*
* $Id: d510m.F,v 1.2 1997/04/07 10:01:05 mclareni Exp $
*
* $Log: d510m.F,v $
* Revision 1.2  1997/04/07 10:01:05  mclareni
* Mods for winnt
*
* Revision 1.1.1.1.2.1  1997/01/21 11:26:23  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/04/01 15:01:25  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D510M
#include "iorc.inc"
      COMMON/G/G(100)/Z/Z(2485)/A/A(100)/PL/PL(100)/SIGMA/SIGMA(100)
     2/AU/AMX(100)/AL/AMN(100)/ENDFLG/ENDFLG,NA,INDFLG(5)
     1/R/R(100)/DA/DA(100)/EXDA/EXDA(1500)/NED/NED(2)/DF/DF(100)

C-----FUMILI TEST-CASE
C-----NO. OF PARAMETERS
      M=3
C-----NO. OF REDUCTIONS IN STEP PER ITERATION
      N1=1
C-----NO. OF ITERATIONS BEFORE INCREASE IN STEP SIZE
      N2=1
C-----MAXIMUM NUMBER OF ITERATIONS
      N3=20
C-----CRITERION FOR MINIMUM
      EPS=0.1
C-----PRINTOUT FREQUENCY
      IT=1
C-----NUMBER OF DATA POINTS
      NED(1)=17
C-----NUMBER OF COORDINATES PER DATA POINT + 2
      NED(2)=3
C-----INITIAL VALUES
      A(1)=0.
      A(2)=8.
      A(3)=11.
C-----UPPER LIMITS
      AMX(1)=1200.
      AMX(3)=11.04
C-----LOWER LIMITS
      AMN(2)=4.
C-----INITIAL STEP SIZE
      PL(1)=100.
      PL(2)=4.
      PL(3)=4.
C-----CO-ORDINATES
      EXDA(1)=10.
      EXDA(2)=.02
      EXDA(3)=1.
      EXDA(4)=18.
      EXDA(5)=.025
      EXDA(6)=4.
      EXDA(7)=40.
      EXDA(8)=.02
      EXDA(9)=6.
      EXDA(10)=74.
      EXDA(11)=.125
      EXDA(12)=7.
      EXDA(13)=82.
      EXDA(14)=.0222
      EXDA(15)=8.
      EXDA(16)=136.
      EXDA(17)=.01
      EXDA(18)=9.
      EXDA(19)=168.
      EXDA(20)=.0083
      EXDA(21)=10.
      EXDA(22)=168.
      EXDA(23)=.0143
      EXDA(24)=11.
      EXDA(25)=160.
      EXDA(26)=.0083
      EXDA(27)=12.
      EXDA(28)=130.
      EXDA(29)=.0182
      EXDA(30)=13.
      EXDA(31)=114.
      EXDA(32)=.01
      EXDA(33)=13.5
      EXDA(34)=120.
      EXDA(35)=.0167
      EXDA(36)=14.
      EXDA(37)=81.
      EXDA(38)=.0222
      EXDA(39)=15.
      EXDA(40)=46.
      EXDA(41)=.02
      EXDA(42)=16.
      EXDA(43)=29.
      EXDA(44)=.0222
      EXDA(45)=17.
      EXDA(46)=2.
      EXDA(47)=.04
      EXDA(48)=18.
      EXDA(49)=.0
      EXDA(50)=.05
      EXDA(51)=20.
      DO 1 I=2,50,3
    1 EXDA(I)=1./SQRT(EXDA(I))
      CALL FUMILI(S,M,N1,N2,N3,EPS,AKAPPA,ALAMBD,IT,MC)
      CALL ERRORF(M)
C-----LIKELM TEST-CASE
      M=4
      N1=1
      N2=1
      N3=100
      EPS=0.1
      IT=1
      NED(1)=200
C-----NUMBER OF CO-ORDINATES PER POINT
      NED(2)=1
      A(1)=-1.
      A(2)=62.8
      A(3)=-0.1
      A(4)=1.57
      AMX(1)=-.2
      AMX(2)=65.0
      AMX(3)=0.
      AMX(4)=5.5
      AMN(1)=-2.0
      AMN(2)=55.0
      AMN(3)=-2.0
      AMN(4)=0.0
      PL(1)=1.
      PL(2)=1.
      PL(3)=.1
      PL(4)=.5
C-----FUNCT FLAG
      INDFLG(3)=1
      DO 3 I=1,200
    2 Z(1)=RNDM(-1)
      EXDA(I)=Z(1)
      IF(5.*Z(1).GT.FUNCT(EXDA(I))) GO TO 2
    3 CONTINUE
      CALL LIKELM(S,M,N1,N2,N3,EPS,AKAPPA,ALAMBD,IT,MC)
      END
      BLOCK DATA
      COMMON/AU/AMX(100)/AL/AMN(100)
#ifdef CERNLIB_IBM
      DATA AMN,AMX/100*-1.E75,100*1.E75/
#else
      DATA AMN,AMX/100*-1.E35,100*1.E35/
#endif
      END

