C***********************************************************************
C $Id: arradq.F,v 1.2 1996/04/10 12:33:34 mclareni Exp $

      SUBROUTINE ARRADQ(ID)

C...ARiadne subroutine RADiate Q-qbar pair

C...Performs the emission of a q-qbar pair from gluon in dipole ID

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "ardat1.f"
#include "arint2.f"

      INXT(I)=IDO(IP3(I))
      IPRV(I)=IDI(IP1(I))

C...Boost dipole to its CMS and copy its invariant mass^2
      CALL ARBOCM(ID)
      BS=ARMAS2(IP1(ID),IP3(ID))
      IF (ABS(BS-SDIP(ID)).GT.(BS+SDIP(ID))*PARA(39).AND.
     $     MSTA(9).GE.2) CALL ARERRM('ARRADQ',13,0)

C...Check which gluon to split
      IF (IRAD(ID).LT.0) THEN
C.......Determine patons ability to recoil, save pointers and flag
C.......affected dipoles
        QR1=.TRUE.
        QR3=(QQ(IP3(ID)).AND.MSTA(16).GT.0)
        IPG=IP1(ID)
        IDN=ID
        IDP=IPRV(ID)
        IF (INXT(ID).NE.0) QDONE(INXT(ID))=.FALSE.
C.......Split the gluon entry, orientate the partons, and boost back
        CALL ARSPLG(IPG,ABS(IRAD(ID)))
        CALL ARORIE(IP3(IDP),IP1(IDN),IP3(IDN),BS,BX1(ID),BX3(ID),
     $              QR1,QR3,0.0,0.0)
        CALL AROBO3(THE,PHI,DBEX,DBEY,DBEZ,
     $              IP3(IDP),IP1(IDN),IP3(IDN))
      ELSE
C.......Determine patons ability to recoil, save pointers and flag
C.......affected dipoles
        QR3=.TRUE.
        QR1=(QQ(IP1(ID)).AND.MSTA(16).GT.0)
        IPG=IP3(ID)
        IDP=ID
        IDN=INXT(ID)
        IF (IPRV(ID).NE.0) QDONE(IPRV(ID))=.FALSE.
C.......Split the gluon entry, orientate the partons, and boost back
        CALL ARSPLG(IPG,ABS(IRAD(ID)))
        CALL ARORIE(IP1(IDP),IP3(IDP),IP1(IDN),BS,BX1(ID),BX3(ID),
     $              QR1,QR3,0.0,0.0)
        CALL AROBO3(THE,PHI,DBEX,DBEY,DBEZ,
     $              IP1(IDP),IP3(IDP),IP1(IDN))
      ENDIF



      RETURN

C**** END OF ARRADQ ****************************************************
      END
