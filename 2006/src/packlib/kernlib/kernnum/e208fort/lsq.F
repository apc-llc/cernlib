*
* $Id: lsq.F,v 1.1.1.1 1996/02/15 17:48:36 mclareni Exp $
*
* $Log: lsq.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:36  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE LSQ(N,X,Y,M,A)
C
C  LEAST SQUARES POLYNOMIAL FIT WITHOUT WEIGHTS.
C  (E.KEIL.  REVISED BY B.SCHORR, 23.10.1981.)
C            REVISED BY H.LIPPS,  17.10.1984.)
C
          REAL          X(9), Y(9), A(9)
#if !defined(CERNLIB_NUMRDBLE)
          REAL                ZERO, ONE, POWER, XK, YK, B(20,20)
          DATA                ZERO, ONE  / 0., 1. /
#endif
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    ZERO, ONE, POWER, XK, YK, B(20,20)
          DOUBLE PRECISION    DA(20)
          DATA                ZERO, ONE  / 0.D0, 1.D0 /
#endif
          DATA                IDIM       / 20 /
          IF(M .GT. 2)  GOTO 30
          IF(M .LT. 2)  GOTO 10
          CALL LLSQ(N,X,Y,A(1),A(2),IFAIL)
          GOTO 100
  10      IF(M .LT. 1  .OR.  N .LT. 1)  GOTO 900
          A(1)  =  RVSUM(N,Y(1),Y(2)) / FLOAT(N)
          RETURN
  30      IF(M .GT. IDIM  .OR.  M .GT. N)  GOTO 900
          B(1,1)=FLOAT(N)
#if !defined(CERNLIB_NUMRDBLE)
          A(1)=ZERO
#endif
#if defined(CERNLIB_NUMRDBLE)
          DA(1)=ZERO
#endif
          DO 1 L=2,M
             B(L,1)=ZERO
             B(M,L)=ZERO
#if !defined(CERNLIB_NUMRDBLE)
             A(L)=ZERO
#endif
#if defined(CERNLIB_NUMRDBLE)
             DA(L)=ZERO
#endif
    1        CONTINUE
          DO 4 K=1,N
             XK=X(K)
             YK=Y(K)
             POWER=ONE
#if !defined(CERNLIB_NUMRDBLE)
             A(1)=A(1)+YK
#endif
#if defined(CERNLIB_NUMRDBLE)
             DA(1)=DA(1)+YK
#endif
             DO 2 L=2,M
                POWER=POWER*XK
                B(L,1)=B(L,1)+POWER
#if !defined(CERNLIB_NUMRDBLE)
                A(L)=A(L)+POWER*YK
#endif
#if defined(CERNLIB_NUMRDBLE)
                DA(L)=DA(L)+POWER*YK
#endif
    2           CONTINUE
             DO 3 L=2,M
                POWER=POWER*XK
                B(M,L)=B(M,L)+POWER
    3           CONTINUE
    4        CONTINUE
          DO 6 I=3,M
             DO 5 K=I,M
                B(K-1,I-1)=B(K,I-2)
    5           CONTINUE
    6        CONTINUE
#if !defined(CERNLIB_NUMRDBLE)
          CALL RSEQN(M,B,IDIM,IFAIL,1,A)
#endif
#if defined(CERNLIB_NUMRDBLE)
          CALL DSEQN(M,B,IDIM,IFAIL,1,DA)
          DO 9 I=1,M
             A(I)=DA(I)
    9        CONTINUE
#endif
 100      IF(IFAIL .EQ. 0)  RETURN
 900      CALL E208PR(N,M,IDIM)
          RETURN
          END
