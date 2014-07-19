*
* $Id: arduph.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
*
* $Log: arduph.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:01  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arduph.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
 
      SUBROUTINE ARDUPH
 
C...ARiadne subroutine DUmp PHoton
 
C...Moves photon emitted by Ariadne to /LUJETS/
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      COMMON /ARINT3/ DPTOT(5)
      SAVE /ARINT3/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
 
 
      N=N+1
      DO 100 I=1,5
        P(N,I)=BP(IPART+1,I)
        DPTOT(I)=DPTOT(I)-BP(IPART+1,I)
        V(N,I)=V(IMF,I)
 100  CONTINUE
 
      DPTOT(5)=DSQRT(DPTOT(4)**2-DPTOT(3)**2-DPTOT(2)**2-DPTOT(1)**2)
 
      K(N,1)=1
      K(N,2)=22
      K(N,3)=IMF
      K(N,4)=0
      K(N,5)=IO
 
      RETURN
 
C**** END OF ARDUPH ****************************************************
      END
