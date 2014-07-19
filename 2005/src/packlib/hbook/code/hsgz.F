*
* $Id: hsgz.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hsgz.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HSGZ (TFUNC,M,S,Z,G,A,DF,EXDA,PL0,AMX,AMN)
*.==========>
*.           HSGZ SETS UP S(OBJECTIVE MIN. FUNCTION),
*.           G(GRADIENT OF S) AND Z(APPROXIMATE COVARIANCE MATRIX)
*..=========> ( I.Ivanchenko )
      DIMENSION Z(1),G(1),A(1),DF(1),EXDA(1),PL0(1),AMX(1),AMN(1)
#include "hbook/hcfit1.inc"
#include "hbook/hcfit2.inc"
#include "hbook/hcflag.inc"
      EXTERNAL TFUNC
*.___________________________________________
      NUMEP1=0
      K=NUMEP
      K2=1
      DO 18 L1=1,K
         K1=K2
         IF(IFLBUF.EQ.0)K1=1
         NX=IDIMPN-2
*
         IF(IFLBUF.EQ.1)GO TO 5
         CALL HEXDA1(EXDA,L1,IFLRET)
         IF(IFLRET.EQ.0)GO TO 18
*
    5    CONTINUE
         NUMEP1=NUMEP1+1
         DO 6 I=1,NX
            KI=K1+1+I
            XFUMIL(I)=EXDA(KI)
    6    CONTINUE
    7    CONTINUE
         IF(IFLFUN.EQ.2)Y=TFUNC(XFUMIL)
         IF(IFLFUN.EQ.1.AND.IDER.EQ.0)CALL HARITH(TFUNC,Y,A,DF,PL0,AMX,
     +   AMN)
         IF(IFLFUN.EQ.2.OR.IDER.EQ.0)GO TO 9
         IF(IDIMPN.EQ.4)GO TO 8
         Y=TFUNC(XFUMIL)
         CALL HDERI1(ID,XFUMIL,A,DF)
         GO TO 9
    8    XHFIT=XFUMIL(1)
         YHFIT=XFUMIL(2)
         CALL HTFUNC(TFUNC,XHFIT,YHFIT,Y)
*
         CALL HDERI2(ID,XHFIT,YHFIT,A,DF)
    9    CONTINUE
*
*             CHI SQUARED
*
   11    SIG=EXDA(K1+1)
         Y=Y-EXDA(K1)
         S=S+((Y/SIG)**2)/2.
   12    CONTINUE
         N=0
         DO 14 J=1,M
            IF (PL0(J)) 14,14,13
   13       N=N+1
            DF(N)=DF(J)/SIG
            G(J)=G(J)+DF(N)*(Y/SIG)
   14    CONTINUE
         L=1
         IF (N.LT.1) GO TO 17
         DO 16 I=1,N
            DO 16 J=1,I
               Z(L)=Z(L)+DF(I)*DF(J)
               L=L+1
   16    CONTINUE
   17    K2=K2+IDIMPN
   18 CONTINUE
      END
