*
* $Id: ltred2.F,v 1.1.1.1 1996/03/01 11:38:38 mclareni Exp $
*
* $Log: ltred2.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:38  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.13/01 05/03/92  17.11.52  by  Rene Brun
*-- Author :
      SUBROUTINE LTRED2(NM,N,D,E,Z)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION D,E,Z,F,G,H,HH,SCALE
#endif
      DIMENSION D(N),E(N),Z(NM,N)
C
      IF (N .EQ. 1) GO TO 320
      DO 300 II = 2, N
         I = N + 2 - II
         L = I - 1
         H = 0.0
         SCALE = 0.0
         IF (L .LT. 2) GO TO 130
         DO 120 K = 1, L
  120    SCALE = SCALE +  ABS(Z(I,K))
         IF (SCALE .NE. 0.0) GO TO 140
  130    E(I) = Z(I,L)
         GO TO 290
  140    DO 150 K = 1, L
            Z(I,K) = Z(I,K) / SCALE
            H = H + Z(I,K) * Z(I,K)
  150    CONTINUE
         F = Z(I,L)
#if defined(CERNLIB_DOUBLE)
         G = -DSIGN(DSQRT(H),F)
#endif
#if !defined(CERNLIB_DOUBLE)
         G = - SIGN( SQRT(H),F)
#endif
         E(I) = SCALE * G
         H = H - F * G
         Z(I,L) = F - G
         F = 0.0
         DO 240 J = 1, L
            Z(J,I) = Z(I,J) / (SCALE * H)
            G = 0.0
            DO 180 K = 1, J
  180       G = G + Z(J,K) * Z(I,K)
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
            DO 200 K = JP1, L
  200       G = G + Z(K,J) * Z(I,K)
  220       E(J) = G / H
            F = F + E(J) * Z(I,J)
  240    CONTINUE
         HH = F / (H + H)
         DO 260 J = 1, L
            F = Z(I,J)
            G = E(J) - HH * F
            E(J) = G
            DO 260 K = 1, J
               Z(J,K) = Z(J,K) - F * E(K) - G * Z(I,K)
  260    CONTINUE
         DO 280 K = 1, L
  280    Z(I,K) = SCALE * Z(I,K)
  290    D(I) = H
  300 CONTINUE
  320 D(1) = 0.0
      E(1) = 0.0
      DO 500 I = 1, N
         L = I - 1
         IF (D(I) .EQ. 0.0) GO TO 380
         DO 360 J = 1, L
            G = 0.0
            DO 340 K = 1, L
  340       G = G + Z(I,K) * Z(K,J)
            DO 360 K = 1, L
               Z(K,J) = Z(K,J) - G * Z(K,I)
  360    CONTINUE
  380    D(I) = Z(I,I)
         Z(I,I) = 1.0
         IF (L .LT. 1) GO TO 500
         DO 400 J = 1, L
            Z(I,J) = 0.0
            Z(J,I) = 0.0
  400    CONTINUE
  500 CONTINUE
      RETURN
      END
