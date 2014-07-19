*
* $Id: palsq.F,v 1.1.1.1 1996/03/01 11:38:41 mclareni Exp $
*
* $Log: palsq.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:41  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.14/00 02/07/92  14.22.32  by  O.Couet
*-- Author :
          SUBROUTINE PALSQ(N,X,Y,M,A)
*.==========>
*.           Auxiliary to PAFITV
*            Extracted from CERN Program library routine LSQ
*.
*.  Least squares polynomial fitting without weights
*.  (E.Keil.  revised by B.Schorr, 23.10.1981.)
*..=========> ( R.Brun from CERNLIB )
          REAL           A(*),X(*),Y(*)
#if !defined(CERNLIB_DOUBLE)
          REAL                ZERO, ONE, POWER, XK, YK, B(20,20)
          DATA                ZERO, ONE  / 0., 1. /
#endif
#if defined(CERNLIB_DOUBLE)
          DOUBLE PRECISION    ZERO, ONE, POWER, XK, YK, B(20,20)
          DOUBLE PRECISION    DA(20)
          DATA                ZERO, ONE  / 0.D0, 1.D0 /
#endif
          DATA                IDIM       / 20 /
          IF(M .GT. 2)  GOTO 30
          CALL PALLSQ(N,X,Y,A(1),A(2),IFAIL)
          GOTO 999
  30      IF(M .GT. IDIM  .OR.  M .GT. N)  GOTO 999
          B(1,1)=FLOAT(N)
#if !defined(CERNLIB_DOUBLE)
          A(1)=ZERO
#endif
#if defined(CERNLIB_DOUBLE)
          DA(1)=ZERO
#endif
          DO 1 L=2,M
             B(L,1)=ZERO
             B(M,L)=ZERO
#if !defined(CERNLIB_DOUBLE)
             A(L)=ZERO
#endif
#if defined(CERNLIB_DOUBLE)
             DA(L)=ZERO
#endif
    1        CONTINUE
          DO 4 K=1,N
             XK=X(K)
             YK=Y(K)
             POWER=ONE
#if !defined(CERNLIB_DOUBLE)
             A(1)=A(1)+YK
#endif
#if defined(CERNLIB_DOUBLE)
             DA(1)=DA(1)+YK
#endif
             DO 2 L=2,M
                POWER=POWER*XK
                B(L,1)=B(L,1)+POWER
#if !defined(CERNLIB_DOUBLE)
                A(L)=A(L)+POWER*YK
#endif
#if defined(CERNLIB_DOUBLE)
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
#if !defined(CERNLIB_DOUBLE)
          CALL HSEQNR(M,B,IDIM,IFAIL,1,A)
#endif
#if defined(CERNLIB_DOUBLE)
          CALL HSEQND(M,B,IDIM,IFAIL,1,DA)
          DO 9 I=1,M
             A(I)=DA(I)
    9        CONTINUE
#endif
 999      RETURN
          END

