*
* $Id: hseqnr.F,v 1.2 1996/02/20 16:47:42 cernlib Exp $
*
* $Log: hseqnr.F,v $
* Revision 1.2  1996/02/20 16:47:42  cernlib
* Merge hseqnr.F and hseqnd.F into hseqnr.F;delete hseqnd.F.
*  These routines were identical, except for singe/double precision, and
*  only one of them is used on a given machine.
*
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
#if defined(CERNLIB_DOUBLE)
      SUBROUTINE HSEQND(N,A,IDIM,IFAIL,K,B)
#else
      SUBROUTINE HSEQNR(N,A,IDIM,IFAIL,K,B)
#endif
*.==========>
*.           Auxiliary to HFITPO/HFITEX. Called by HLSQ
*            Extracted from CERN Program library routine RSEQN
*..=========> ( R.Brun from CERNLIB )

#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION A(IDIM,*), B(IDIM,*), ONE
      DOUBLE PRECISION S1, S21, S22
#else
      REAL A(IDIM,*), B(IDIM,*), ONE
      REAL S1, S21, S22
#endif
      DATA ONE / 1. /
      IF(IDIM .LT. N) GOTO 999
*SEQ,     SFACT.
      IFAIL = 0
      DO 144 J = 1, N
         IF(A(J,J) .LE. 0.) GOTO 150
         A(J,J) = ONE / A(J,J)
         IF(J .EQ. N) GOTO 199
  140    JP1 = J+1
         DO 143 L = JP1, N
            A(J,L) = A(J,J)*A(L,J)
            S1 = -A(L,J+1)
            DO 141 I = 1, J
               S1 = A(L,I)*A(I,J+1)+S1
  141       CONTINUE
            A(L,J+1) = -S1
  143    CONTINUE
  144 CONTINUE
  150 IFAIL = -1
      RETURN
  199 CONTINUE
*SEQ,     SFEQN.
      IF(K .LE. 0) GOTO 999
      DO 220 L = 1, K
         B(1,L) = A(1,1)*B(1,L)
  220 CONTINUE
      IF(N .EQ. 1) GOTO 999
      DO 243 L = 1, K
         DO 232 I = 2, N
            IM1 = I-1
            S21 = - B(I,L)
            DO 231 J = 1, IM1
               S21 = A(I,J)*B(J,L)+S21
  231       CONTINUE
            B(I,L) = - A(I,I)*S21
  232    CONTINUE
         NM1 = N-1
         DO 242 I = 1, NM1
            NMI = N-I
            S22 = - B(NMI,L)
            DO 241 J = 1, I
               NMJP1 = N - J+1
               S22 = A(NMI,NMJP1)*B(NMJP1,L)+S22
  241       CONTINUE
            B(NMI,L) = - S22
  242    CONTINUE
  243 CONTINUE
  999 RETURN
      END

