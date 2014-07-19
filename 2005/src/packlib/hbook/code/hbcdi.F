*
* $Id: hbcdi.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbcdi.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 05/03/89  09.18.58  by  Rene Brun
*-- Author :
      SUBROUTINE HBCDI(L,N,IA)
*.==========>
*.            this routine converts integer in BCD
*..=========> ( R.Brun )
      DIMENSION IA(1)
#include "hbook/hcprin.inc"
*.___________________________________________
      IF(L.EQ.0)THEN
         DO 10 I=1,N
  10     IA(I)=IDG(41)
         RETURN
      ENDIF
*
      M=L
      NN=N
      IA(1)=IDG(41)
      IF(L.LT.0)THEN
         M=-L
         IA(1)=IDG(39)
         NN=N-1
      ENDIF
      N1=1
      J=N+1
      K1=0
      DO 40 I=1,NN
         J=J-1
         IF(M.GE.N1)THEN
            N2=10*N1
            K2=MOD(M,N2)
            K=(K2-K1)/N1
            IA(J)=IDG(K+1)
         ELSE
            IA(J)=IDG(41)
         ENDIF
         K1=K2
         N1=N2
  40  CONTINUE
      END
