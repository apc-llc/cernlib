*
* $Id: b300m.F,v 1.2 2006/09/15 09:34:55 mclareni Exp $
*
* $Log: b300m.F,v $
* Revision 1.2  2006/09/15 09:34:55  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE B300M
C     Routine to test MATHLIB routines RSRTNT and DSRTNT   (B300)

#include "gen/imp64.inc"
      LOGICAL LRL
      LOGICAL LTEST
      EXTERNAL FB300
      COMMON /FOB300/ A,B,C
      COMMON /F1B300/ K,N
      PARAMETER (R0 = 0, R1 = 1, NR = 5)
      DIMENSION RU(NR),RV(NR)

#include "iorc.inc"
      P(X)=A+B*X+C*X**2

      CALL HEADER('B300',0)
      LTEST= .TRUE.
#if defined(CERNLIB_DOUBLE)
      CALL DVRAN(NR,-3D0,3D0,RU(1),RU(2))
      CALL DVRAN(NR,-3D0,3D0,RV(1),RV(2))
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RVRAN(NR,-3D0,3D0,RU(1),RU(2))
      CALL RVRAN(NR,-3D0,3D0,RV(1),RV(2))
#endif
      DO 1 K = -3,3
      DO 1 N = 1,3,2
      DO 1 IA = -2,2
      A=IA
      DO 1 IB = -2,2
      B=IB
      DO 1 IC = -2,2
      C=IC
      IF(ABS(A)+ABS(B)+ABS(C) .GT. 0) THEN
       DELTA=4*A*C-B**2
       DO 2 IU = 1,NR
       U=RU(IU)
       DO 2 IV = 1,NR
       V=RV(IV)
#if defined(CERNLIB_DOUBLE)
       CALL DSRTNT(K,N,A,B,C,U,V,RES,LRL)
#endif
#if !defined(CERNLIB_DOUBLE)
       CALL RSRTNT(K,N,A,B,C,U,V,RES,LRL)
#endif
       IF(LRL) THEN
        U1=MIN(U,V)
        V1=MAX(U,V)
        SGN=SIGN(R1,V-U)
#if defined(CERNLIB_DOUBLE)
        IF(K .NE. -1) TST=SGN*DGAUSS(FB300,U1,V1,1D-8)
        IF(K .EQ. -1) TST=SGN*DCAUCH(FB300,U1,V1,R0,1D-8)
#endif
#if !defined(CERNLIB_DOUBLE)
        IF(K .NE. -1) TST=SGN*GAUSS(FB300,U1,V1,1D-8)
        IF(K .EQ. -1) TST=SGN*CAUCHY(FB300,U1,V1,R0,1D-8)
#endif
        D=RES-TST
        LTEST = LTEST .AND. ABS(D) .LE.1D-7
* 
* gfortran hates logical in format statement:-(
*
#if defined(CERNLIB_GFORTRAN)
        IF(ABS(D) .GT. 1D-7)
     1  WRITE(LOUT,'(1X,2I5,F10.1,3F7.1,2F8.4,3F15.8,'' LB'',L2)')
     2           K,N,DELTA,A,B,C,U,V,RES,TST,D,LRL
#else
        IF(ABS(D) .GT. 1D-7)
     1  WRITE(LOUT,'(1X,2I5,F10.1,3F7.1,2F8.4,3F15.8,'' LB'',I2,L2)')
     2           K,N,DELTA,A,B,C,U,V,RES,TST,D,LRL
#endif
       ENDIF
    2  CONTINUE
      ENDIF
    1 CONTINUE
      WRITE(LOUT,'(/1X,''If the test is successful, the results will not
     1 be printed.''/)')
      WRITE(LOUT,'(/7X,''TESTING ERROR MESSAGES:''/)')
#if defined(CERNLIB_DOUBLE)
      CALL DSRTNT(K,N,R0,R0,R0,U,V,RES,LRL)
      CALL DSRTNT(5,1,R1,R1,R1,U,V,RES,LRL)
      CALL DSRTNT(0,2,R1,R1,R1,U,V,RES,LRL)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RSRTNT(K,N,R0,R0,R0,U,V,RES,LRL)
      CALL RSRTNT(5,1,R1,R1,R1,U,V,RES,LRL)
      CALL RSRTNT(0,2,R1,R1,R1,U,V,RES,LRL)
#endif
      WRITE(LOUT,'(1X)')
      IRC=ITEST('B300',LTEST)
      CALL PAGEND('B300')
      RETURN
      END
      FUNCTION FB300(T)
#include "gen/imp64.inc"
      COMMON /FOB300/ A,B,C
      COMMON /F1B300/ K,N

      FB300=T**K/SQRT(A+B*T+C*T**2)**N
      RETURN
      END
