*
* $Id: arradq.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arradq.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arradq.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      SUBROUTINE ARRADQ(ID)
 
C...ARiadne subroutine RADiate Q-qbar pair
 
C...Performs the emission of a q-qbar pair from gluon in dipole ID
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),IDIPS
      SAVE /ARDIPS/
 
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      COMMON /ARINT2/ DBEX,DBEY,DBEZ,PHI,THE
      SAVE /ARINT2/
 
      INXT(I)=IDO(IP3(I))
      IPRV(I)=IDI(IP1(I))
 
C...Boost dipole to its CMS and copy its invariant mass^2
      CALL ARBOCM(ID)
      BS=ARMAS2(IP1(ID),IP3(ID))
      IF(ABS(BS-SDIP(ID)).GT.(BS+SDIP(ID))*PARA(39).AND.
     $     MSTA(9).GE.2) CALL ARERRM('ARRADQ',13,0)
 
C...Check which gluon to split
      IF(IRAD(ID).LT.0) THEN
C.......Determine patons ability to recoil, save pointers and flag
C.......affected dipoles
        QR1=.TRUE.
        QR3=(QQ(IP3(ID)).AND.MSTA(16).GT.0)
        IPG=IP1(ID)
        IDN=ID
        IDP=IPRV(ID)
        IF(INXT(ID).NE.0) QDONE(INXT(ID))=.FALSE.
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
        IF(IPRV(ID).NE.0) QDONE(IPRV(ID))=.FALSE.
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
