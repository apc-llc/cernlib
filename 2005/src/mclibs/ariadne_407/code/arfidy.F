*
* $Id: arfidy.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arfidy.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arfidy.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      SUBROUTINE ARFIDY(NSAVE)
 
C...ARiadne subroutine FInish up Drell-Yan event
 
C...Finish Drell-Yan event performing cascade on D-Y decay products
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /PYPARS/ MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/
 
 
      IF (.NOT.QQ(MAXPAR-2)) RETURN
      QQ(MAXPAR-2)=.FALSE.
      IFIRST=MSTI(4)+1
 
C...Activate D-Y Boson decay products, boost to new Boson momenta and
C...Perform possible cascade.
      IDY=IDI(MAXPAR-2)
      DYE=P(IDY,4)
      DYBZ=SQRT(P(IDY,1)**2+P(IDY,2)**2+P(IDY,3)**2)/DYE
      PHIDY=ULANGL(P(IDY,1),P(IDY,2))
      THEDY=ULANGL(P(IDY,3),SQRT(P(IDY,1)**2+P(IDY,2)**2))
      DO 210 I=K(IDY,3),NSAVE
        IF (I.EQ.IDY) THEN
          IF (K(I,1).GE.100) K(I,1)=K(I,1)-100
        ELSEIF (K(I,1).GE.100) THEN
          K(I,1)=K(I,1)-100
          CALL LUDBRB(I,I,0.0,0.0,0.0D0,0.0D0,DYBZ)
          CALL LUDBRB(I,I,THEDY,PHIDY,0.0D0,0.0D0,0.0D0)
        ENDIF
 210  CONTINUE
      CALL ARPARS(IFIRST,NSAVE)
 
      RETURN
 
C**** END OF ARFIDY ****************************************************
      END
