*
* $Id: arincr.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arincr.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arincr.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      SUBROUTINE ARINCR(IDIR,IQR,IDR,IRP)
 
C...ARiadne subroutine FIx REmnants
 
C...Redistribute remnants and prepare for BGF-like emission
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
 
 
      DIMENSION IPRT(4)
 
      IF (IDIR.GT.0) THEN
        IF(PHAR(131).LE.0) RETURN
        BPX=PHAR(131)
      ELSEIF (IDIR.LT.0) THEN
        IF(PHAR(132).LE.0) RETURN
        BPX=PHAR(132)
      ENDIF
 
      NPRT=0
      BPR=0.0
      IF (IQR.GT.0) THEN
        BPR=BPR+BP(IQR,4)+IDIR*BP(IQR,3)
        NPRT=NPRT+1
        IPRT(NPRT)=IQR
      ENDIF
      IF (IDR.GT.0) THEN
        BPR=BPR+BP(IDR,4)+IDIR*BP(IDR,3)
        NPRT=NPRT+1
        IPRT(NPRT)=IDR
      ENDIF
      IF (IRP.GT.0) THEN
        BPR=BPR+P(IRP,4)+IDIR*P(IRP,3)
      ENDIF
 
      DPN2=(BPR+BPX)**2
      DPO2=BPR**2
      DBZ=IDIR*(DPN2-DPO2)/(DPN2+DPO2)
 
      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,DBZ,NPRT,IPRT)
      IF (IRP.GT.0)
     $     CALL LUDBRB(IRP,IRP,0.0,0.0,0.0D0,0.0D0,DBZ)
 
      RETURN
      END
