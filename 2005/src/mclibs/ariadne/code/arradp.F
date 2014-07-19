C***********************************************************************
C $Id: arradp.F,v 1.2 1996/04/10 12:33:34 mclareni Exp $

      SUBROUTINE ARRADP(ID)

C...ARiadne subroutine RADiate Photon

C...Performs the radiation of a photon from EM-dipole ID

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "ardat1.f"
#include "arint2.f"

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
