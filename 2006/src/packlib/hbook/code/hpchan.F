*
* $Id: hpchan.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hpchan.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 07/03/89  11.05.37  by  Rene Brun
*-- Author :
      SUBROUTINE HPCHAN(CCHAR,IMIN,IMAX,NN,A)
*.==========>
*.             This routine prints the channel numbers
*.           MSTEP = Number of columns for a channel
*.           CCHAR=1H  no UNDER,OVER printed
*.           CCHAR=1H1 only UNDER printed
*.           CCHAR=1H2 only OVER  printed
*.           CCHAR=Anything except 1H ,1H1,1H2 UNDER,OVER printed
*..=========> ( R.Brun )
#include "hbook/hcunit.inc"
#include "hbook/hcprin.inc"
      DIMENSION A(NN),UNDER(5),OVER(5),FLAG(8)
      DIMENSION C(15)
      INTEGER A,CCHAR,UNDER,OVER,C,FLAG
      SAVE UNDER,OVER,C
      DATA UNDER/1HU,1HN,1HD,1HE,1H /
      DATA OVER/1HO,1HV,1HE,1HR,1H /
      DATA C/1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H ,
     +        1H ,1H ,1H ,1H ,1H /
*.___________________________________________
      CALL UCTOH('CHANNELS',FLAG,1,8)
      MST=MSTEP
      IF(CCHAR.EQ.IDG(37))MST=1
      K=1000
      L=0
      N=0
      CALL VBLANK(A,NN)
      KMAX=IMIN-1+(IMAX-IMIN+1)/MSTEP
  10  IF(K.GT.KMAX)GO TO 50
      N1=-1
      L=L+1
      N=N+1
      M=0
      DO 20 I=IMIN,IMAX
         M=M+MSTEP
         IF(M.GT.NN)GO TO 22
         J=MOD(I/K,10)+1
         IF(J.NE.N1)A(M)=IDG(J)
         N1=J
  20  CONTINUE
*
  22  IF(CCHAR.EQ.UNDER(5).OR.CCHAR.EQ.IDG(37))THEN
         L=5
         N=5
      ENDIF
      IF(CCHAR.EQ.IDG(2))L=5
      IF(CCHAR.EQ.IDG(3))N=5
      WRITE(LOUT,1000)FLAG,K,(C(J1),J1=1,MST),UNDER(N),C(1),A,
     +(C(J2),J2=1,MST),OVER(L),(C(J3),J3=1,MST)
      CALL VBLANK(FLAG,8)
  50  K=K/10
      IF(K.GT.0)GO TO 10
*
 1000 FORMAT(' ',8A1,I4,115A1)
      END
