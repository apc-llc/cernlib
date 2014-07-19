*
* $Id: e408m.F,v 1.5 1998/11/05 10:41:34 mclareni Exp $
*
* $Log: e408m.F,v $
* Revision 1.5  1998/11/05 10:41:34  mclareni
* Use cpp defines instead of statement functions for Linux also
*
* Revision 1.4  1997/10/24 14:48:23  mclareni
* For NT, use cpp macro instead of statement function
*
* Revision 1.3  1997/09/02 16:09:42  mclareni
* WINNT corrections
*
* Revision 1.2  1997/04/07 10:01:06  mclareni
* Mods for winnt
*
* Revision 1.1.1.1.2.1  1997/01/21 11:26:23  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/04/01 15:01:26  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE E408M
C     E408  RCHPWS, DCHPWS,  RPWCHS, DPWCHS
#if defined(CERNLIB_DOUBLE)
#include "gen/imp64.inc"
#endif

#include "gen/def128.inc"
     +      QX,QA,QB,Q1

#include "iorc.inc"

      LOGICAL LOK
      CHARACTER*50 TXT(3)

      DIMENSION QB(0:100),CH(0:100),PW(0:100),CH1(0:100),PW1(0:100)

      PARAMETER (R1 = 1, Q1 = 1)

      DATA TXT(1) /'EXP(X)      (-1 <= X <= 1)  LUKE 3.2.2(4)'/
      DATA TXT(2) /'EXP(-X**2)  (-1 <= X <= 1)  LUKE 3.2.2(8)'/
      DATA TXT(3) /'J_1(X)      (-1 <= X <= 1)  LUKE 9.7(7)'/

#if !defined(CERNLIB_WINNT) && !defined(CERNLIB_LINUX)
# if defined(CERNLIB_DOUBLE)
      GVSUM(N,B,C) = DVSUM(N,B,C)
      GGAMMA(A)    = DGAMMA(A)
# endif
# if !defined(CERNLIB_DOUBLE)
      GVSUM(A,B,C) = RVSUM(A,B,C)
      GGAMMA(A)    = GAMMA(A)
# endif 
#else
# if defined(CERNLIB_DOUBLE)
#  define  GVSUM(N,B,C)  DVSUM(N,B,C)
#  define  GGAMMA(A)     DGAMMA(A)
# endif
# if !defined(CERNLIB_DOUBLE)
#  define  GVSUM(N,B,C)  RVSUM(N,B,C)
#  define  GGAMMA(A)     GAMMA(A)
# endif 
#endif /* CERNLIB_WINNT */

      CALL HEADER('E408',0)

#if (defined(CERNLIB_DOUBLE))&&(defined(CERNLIB_VAXVMS))
      EPS=3D-13
#endif
#if (defined(CERNLIB_DOUBLE))&&(!defined(CERNLIB_VAXVMS))
      EPS=3D-14
#endif
#if !defined(CERNLIB_DOUBLE)
      EPS=3E-14
#endif
      LOK=.TRUE.

      NDIG=18
#if defined(CERNLIB_WINNT)
      EPS=3E-11
#endif
      NL=20
      QX=1
      QA=0
#if (defined(CERNLIB_QUAD))&&(defined(CERNLIB_DOUBLE))
      CALL QBSIA(QX,QA,NL,NDIG,QB)
#endif
#if defined(CERNLIB_NOQUAD)||!defined(CERNLIB_DOUBLE)
      CALL DBSIA(QX,QA,NL,NDIG,QB)
#endif
      CH(0)=QB(0)
      DO 1 N = 1,NL
    1 CH(N)=2*QB(N)
      DO 2 N = 0,NL
    2 PW(N)=1/GGAMMA(N+R1)

#if defined(CERNLIB_DOUBLE)
      CALL DCHPWS(NL,CH,PW1)
      CALL DPWCHS(NL,PW,CH1)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RCHPWS(NL,CH,PW1)
      CALL RPWCHS(NL,PW,CH1)
#endif
      SCP=GVSUM(NL+1,CH(0),CH(1))
      SPC=GVSUM(NL+1,PW(0),PW(1))
      SCP1=GVSUM(NL+1,CH1(0),CH1(1))
      SPC1=GVSUM(NL+1,PW1(0),PW1(1))
      LOK=LOK .AND. ABS(SCP-SCP1)+ABS(SPC-SPC1) .LT. EPS
      WRITE(LOUT,'(A1,5X,A50//)') '1',TXT(1)
      WRITE(LOUT,'(1X,I5,4F25.15)') (N,CH(N),CH1(N),PW(N),PW1(N),N=0,NL)
      WRITE(LOUT,'(1X)')
      WRITE(LOUT,'(1X,5X,4F25.15)') SCP,SCP1,SPC,SPC1

      NL=40
      QX=Q1/2
      QA=0
      EQA=EXP(-QX)
#if (defined(CERNLIB_QUAD))&&(defined(CERNLIB_DOUBLE))
      CALL QBSIA(QX,QA,NL,NDIG,QB)
#endif
#if defined(CERNLIB_NOQUAD)||!defined(CERNLIB_DOUBLE)
      CALL DBSIA(QX,QA,NL,NDIG,QB)
#endif
      CH(0)=EQA*QB(0)
      DO 3 N = 1,NL
      IF(MOD(N,2) .EQ. 0) CH(N)=EQA*2*(-1)**(N/2)*QB(N/2)
      IF(MOD(N,2) .EQ. 1) CH(N)=0
    3 CONTINUE
      DO 4 N = 0,NL
      IF(MOD(N,2) .EQ. 0) PW(N)=(-1)**(N/2)/GGAMMA(N/2+R1)
      IF(MOD(N,2) .EQ. 1) PW(N)=0
    4 CONTINUE

      WRITE(LOUT,'(1X)')
#if defined(CERNLIB_DOUBLE)
      CALL DCHPWS(NL,CH,PW1)
      CALL DPWCHS(NL,PW,CH1)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RCHPWS(NL,CH,PW1)
      CALL RPWCHS(NL,PW,CH1)
#endif
      SCP=GVSUM(NL+1,CH(0),CH(1))
      SPC=GVSUM(NL+1,PW(0),PW(1))
      SCP1=GVSUM(NL+1,CH1(0),CH1(1))
      SPC1=GVSUM(NL+1,PW1(0),PW1(1))
      WRITE(LOUT,'(A1,5X,A50//)') '1',TXT(2)
      WRITE(LOUT,'(1X,I5,4F25.15)') (N,CH(N),CH1(N),PW(N),PW1(N),N=0,NL)
      WRITE(LOUT,'(1X)')
      WRITE(LOUT,'(1X,5X,4F25.15)') SCP,SCP1,SPC,SPC1
      LOK=LOK .AND. ABS(SCP-SCP1)+ABS(SPC-SPC1) .LT. EPS

#if !defined(CERNLIB_VAX)
      NL=50
#endif
#if defined(CERNLIB_VAX)
      NL=40
#endif
      QX=4
      QA=0
#if (defined(CERNLIB_QUAD))&&(defined(CERNLIB_DOUBLE))
      CALL QBSJA(QX,QA,NL,NDIG,QB)
#endif
#if defined(CERNLIB_NOQUAD)||!defined(CERNLIB_DOUBLE)
      CALL DBSJA(QX,QA,NL,NDIG,QB)
#endif
      CH(0)=0
      DO 5 N = 1,NL
      IF(MOD(N,2) .EQ. 0) CH(N)=0
      IF(MOD(N,2) .EQ. 1) CH(N)=2*(-1)**(N/2)*QB(N/2)*QB(N/2+1)
    5 CONTINUE
      DO 6 N = 0,NL
      IF(MOD(N,2) .EQ. 0) PW(N)=0
      IF(MOD(N,2) .EQ. 1)
     1 PW(N)=(-1)**(N/2)*QX**N/(GGAMMA(N/2+R1)*GGAMMA(N/2+2*R1))
    6 CONTINUE

      WRITE(LOUT,'(1X)')
#if defined(CERNLIB_DOUBLE)
      CALL DCHPWS(NL,CH,PW1)
      CALL DPWCHS(NL,PW,CH1)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RCHPWS(NL,CH,PW1)
      CALL RPWCHS(NL,PW,CH1)
#endif
      SCP=GVSUM(NL+1,CH(0),CH(1))
      SPC=GVSUM(NL+1,PW(0),PW(1))
      SCP1=GVSUM(NL+1,CH1(0),CH1(1))
      SPC1=GVSUM(NL+1,PW1(0),PW1(1))
      WRITE(LOUT,'(A1,5X,A50//)') '1',TXT(3)
      WRITE(LOUT,'(1X,I5,4F25.15)') (N,CH(N),CH1(N),PW(N),PW1(N),N=0,NL)
      WRITE(LOUT,'(1X)')
      WRITE(LOUT,'(1X,5X,4F25.15)') SCP,SCP1,SPC,SPC1
      LOK=LOK .AND. ABS(SCP-SCP1)+ABS(SPC-SPC1) .LT. EPS

      WRITE(LOUT,'(1X)')
#if defined(CERNLIB_DOUBLE)
      IF(LOK) WRITE(LOUT,
     1 '(7X,''E408 DCHPWS/DPWCHS ** TEST SUCCESSFUL **'')')
      IF(.NOT.LOK) WRITE(LOUT,
     1 '(7X,''E408 DCHPWS/DPWCHS ** TEST FAILED **'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      IF(LOK) WRITE(LOUT,
     1 '(7X,''E408 RCHPWS/RPWCHS ** TEST SUCCESSFUL **'')')
      IF(.NOT.LOK) WRITE(LOUT,
     1 '(7X,''E408 RCHPWS/RPWCHS ** TEST FAILED **'')')
#endif

      IRC= ITEST('E408', LOK )

      WRITE(LOUT,'(1X)')
#if defined(CERNLIB_DOUBLE)
      CALL DCHPWS(-1,CH,PW1)
      CALL DPWCHS(101,PW,CH1)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RCHPWS(-1,CH,PW1)
      CALL RPWCHS(101,PW,CH1)
#endif

      CALL PAGEND('E408')

      END

