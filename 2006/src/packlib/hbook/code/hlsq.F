*
* $Id: hlsq.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hlsq.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HLSQ(N,M,A)
*.==========>
*.           Auxiliary to HFITPO/HFITEX.
*            Extracted from CERN Program library routine LSQ
*.
*.  Least squares lpolynomial fitting without weights
*.  (E.Keil.  revised by B.Schorr, 23.10.1981.)
*..=========> ( R.Brun from CERNLIB )
      REAL A(9),EXDA(4)
      SAVE ZERO,ONE,IDIM
#if !defined(CERNLIB_DOUBLE)
      REAL ZERO, ONE, POWER, XK, YK, B(20,20)
      DATA ZERO, ONE / 0., 1. /
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION ZERO, ONE, POWER, XK, YK, B(20,20)
      DOUBLE PRECISION DA(20)
      DATA ZERO, ONE / 0.D0, 1.D0 /
#endif
      DATA IDIM / 20 /
      IF(M .GT. 2) GOTO 30
      CALL HLLSQ(N,A(1),A(2),IFAIL)
      GOTO 999
   30 IF(M .GT. IDIM .OR. M .GT. N) GOTO 999
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
    1 CONTINUE
      DO 4 K=1,N
         CALL HFITH1(EXDA,K)
         XK=EXDA(3)
         YK=EXDA(1)
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
    2    CONTINUE
         DO 3 L=2,M
            POWER=POWER*XK
            B(M,L)=B(M,L)+POWER
    3    CONTINUE
    4 CONTINUE
      DO 6 I=3,M
         DO 5 K=I,M
            B(K-1,I-1)=B(K,I-2)
    5    CONTINUE
    6 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      CALL HSEQNR(M,B,IDIM,IFAIL,1,A)
#endif
#if defined(CERNLIB_DOUBLE)
      CALL HSEQND(M,B,IDIM,IFAIL,1,DA)
      DO 9 I=1,M
         A(I)=DA(I)
    9 CONTINUE
#endif
  999 RETURN
      END
