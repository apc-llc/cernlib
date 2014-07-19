*
* $Id: hmconv.F,v 1.1.1.1 1996/01/16 17:07:43 mclareni Exp $
*
* $Log: hmconv.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:43  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 05/03/89  09.22.02  by  Rene Brun
*-- Author :
      SUBROUTINE HMCONV (N,Z,PL,R)
*.==========>
*.           INVERTS THE POSITIVE DEFINITE PACKED SYMMETRIC MATRIX Z
*.           BY THE SQUARE-ROOT METHOD
*..=========> ( D.Lienart )
      DIMENSION Z(1),PL(1),R(1)
#include "hbook/hcfit2.inc"
#include "hbook/hcprin.inc"
*
*             MAXIMUM REAL NUMBER AND 10.*MAXIMUM RELATIVE
*             PRECISION
*
      EQUIVALENCE (AM,BIGP)
      SAVE RP
      DATA RP/1.E-14/
*.___________________________________________
      IF (N.LT.1) RETURN
      APS=SQRT(AM/FLOAT(N))
      AP=1./(APS*APS)
      IR=0
      DO 11 I=1,N
   1     IR=IR+1
         IF (PL(IR).LE.0.)GO TO 1
         NI=I*(I-1)/2
         II=NI+I
         K=N+1
         IF (Z(II).LE.RP*ABS(R(IR)).OR.Z(II).LE.AP) GO TO 19
         Z(II)=1./SQRT(Z(II))
         NL=II-1
   3     IF (NL-NI.LE.0)GO TO 5
         Z(NL)=Z(NL)*Z(II)
         IF (ABS(Z(NL)).GE.APS) GO TO 16
         NL=NL-1
         GO TO 3
   5     IF (I-N.GE.0)GO TO 12
   6     K=K-1
         NK=K*(K-1)/2
         NL=NK
         KK=NK+I
         D=Z(KK)*Z(II)
         C=D*Z(II)
         L=K
   7     LL=NK+L
         LI=NL+I
         Z(LL)=Z(LL)-Z(LI)*C
         L=L-1
         NL=NL-L
         IF (L-I) 9,9,7
   8     LL=NK+L
         LI=NI+L
         Z(LL)=Z(LL)-Z(LI)*D
   9     L=L-1
         IF (L.GT.0)GO TO 8
         Z(KK)=-C
         IF (K-I-1.GT.0)GO TO 6
  11  CONTINUE
*
  12  DO 14 I=1,N
         DO 14 K=I,N
            NL=K*(K-1)/2
            D=0.
            DO 13 L=K,N
               LI=NL+I
               LK=NL+K
               D=D+Z(LI)*Z(LK)
               NL=NL+L
  13        CONTINUE
            KI=K*(K-1)/2+I
            Z(KI)=D
  14  CONTINUE
  15  RETURN
  16  K=I+NL-II
      IR=0
      DO 18 I=1,K
  17     IR=IR+1
         IF (PL(IR).LE.0.)GO TO 17
  18  CONTINUE
  19  PL(IR)=-2.
      R(IR)=0.
      INDFLG(1)=IR
      GO TO 15
      END
