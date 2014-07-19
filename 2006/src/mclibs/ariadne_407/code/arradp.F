*
* $Id: arradp.F,v 1.1.1.1 1996/03/08 16:51:03 mclareni Exp $
*
* $Log: arradp.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:03  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arradp.F,v 1.1.1.1 1996/03/08 16:51:03 mclareni Exp $
 
      SUBROUTINE ARRADP(ID)
 
C...ARiadne subroutine RADiate Photon
 
C...Performs the radiation of a photon from EM-dipole ID
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARINT2/ DBEX,DBEY,DBEZ,PHI,THE
      SAVE /ARINT2/
 
      INXT(I)=IDO(IP3(I))
      IPRV(I)=IDI(IP1(I))
 
C...Boost dipole to its CMS, and get its invaiant mass^2
      CALL ARBOCM(ID)
      BS=ARMAS2(IP1(ID),IP3(ID))
      IF (ABS(BS-SDIP(ID)).GT.(BS+SDIP(ID))*PARA(39).AND.
     $     MSTA(9).GE.2) CALL ARERRM('ARRADG',13,0)
 
      QR1=.TRUE.
      QR3=.TRUE.
C...Use position IPART+1 temporarily for the photon and orientate
C...the particles/partons
      BP(IPART+1,5)=0.0
      CALL ARORIE(IP1(ID),IPART+1,IP3(ID),BS,BX1(ID),BX3(ID),
     $            QR1,QR3,0.0,0.0)
 
C...Boost back to original CMS
      CALL AROBO3(THE,PHI,DBEX,DBEY,DBEZ,
     $            IP1(ID),IPART+1,IP3(ID))
C...Copy photon information to /LUJETS/
      CALL ARDUPH
 
C...Flagg dipoles that were affected by the emission
      QDONE(INXT(ID))=.FALSE.
      QDONE(IPRV(ID))=.FALSE.
      QDONE(ID)=.FALSE.
 
      RETURN
 
C**** END OF ARRADP ****************************************************
      END
