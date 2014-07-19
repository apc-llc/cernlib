*
* $Id: c348m.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c348m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C348M
C     This program tests the operation of MATHLIB subprograms
C     CELINT and WELINT

#include "gen/imp64.inc"
#include "gen/defc64.inc"
     + FC348,Z,V,W,I,Z2
#if defined(CERNLIB_DOUBLE)
#include "gen/defc64.inc"
     +WGAUSS,WELINT
#endif
#if !defined(CERNLIB_DOUBLE)
#include "gen/defc64.inc"
     +CGAUSS,CELINT
#endif
      EXTERNAL FC348
      COMMON /FOC348/ AKC2,A,B

      PARAMETER (NR = 5, TSTER=5E-8)
      PARAMETER (Z0 = 0, Z1 = 1, Z10 = 10, HF = Z1/2,I = (0,1),Z2=(0,0))
      DIMENSION RKC(NR),RB(NR),RZR(NR),RZI(NR)
#include "iorc.inc"

      CALL HEADER('C348',0)
      ERMAX=0
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR WELINT'')')
      WRITE(LOUT,'(/6X,''AKP'',8X,''B'',16X,''Z'',26X,''WELINT''
     1,22X,''Error'')')
      CALL DVRAN(NR,-Z10,Z10,RKC(1),RKC(2))
      CALL DVRAN(NR,-Z10,Z10,RB(1),RB(2))
      CALL DVRAN(NR,Z0,Z10,RZR(1),RZR(2))
      CALL DVRAN(NR,-Z10,Z10,RZI(1),RZI(2))
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR CELINT'')')
      WRITE(LOUT,'(/6X,''AKP'',8X,''B'',16X,''Z'',26X,''CELINT''
     1,22X,''Error'')')
      CALL RVRAN(NR,-Z10,Z10,RKC(1),RKC(2))
      CALL RVRAN(NR,-Z10,Z10,RB(1),RB(2))
      CALL RVRAN(NR,Z0,Z10,RZR(1),RZR(2))
      CALL RVRAN(NR,-Z10,Z10,RZI(1),RZI(2))
#endif
      EPS=1D-14
      A=1
      DO 1 IKC = 0,NR
      IF(IKC .EQ. 0) THEN
       AKC=0
      ELSE
       AKC=RKC(IKC)
      ENDIF
      AKC2=AKC**2
      DO 1 IB = 1,NR
      B=RB(IB)
      DO 1 IZ = 1,NR
      Z=RZR(IZ)+I*RZI(IZ)
#if defined(CERNLIB_DOUBLE)
      W=WELINT(Z,AKC,A,B)
      V=WGAUSS(FC348,Z2,Z,EPS)
#endif
#if !defined(CERNLIB_DOUBLE)
      W=CELINT(Z,AKC,A,B)
      V=CGAUSS(FC348,Z2,Z,EPS)
#endif
      E=ABS((W-V)/W)
      ERMAX=MAX(ERMAX,E)
      WRITE(LOUT,'(1X,2F10.4,5X,''('',F7.4,'','',F7.4,'') ('',
     +F19.15,'','',F19.15,'')'',1P,D10.1)')
     +  AKC,B,Z,W,E
    1 CONTINUE
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D10.1)') ERMAX
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if defined(CERNLIB_DOUBLE)
      W=WELINT(-Z1+I*Z1,Z1,Z1,Z1)
#endif
#if !defined(CERNLIB_DOUBLE)
      W=CELINT(-Z1+I*Z1,Z1,Z1,Z1)
#endif
C     Check if the test was successful
      IRC=ITEST('C348',ERMAX .LE. TSTER)
      CALL PAGEND('C348')
      END
      FUNCTION FC348(T)
#include "gen/imp64.inc"
#include "gen/defc64.inc"
     + FC348,T,T1,T2
      COMMON /FOC348/ AKC2,A,B

      T2=T**2
      T1=1+T2
      FC348=(A+B*T2)/(T1*SQRT(T1)*SQRT(1+AKC2*T2))
      RETURN
      END
