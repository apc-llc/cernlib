C***********************************************************************
C $Id: arradg.F,v 1.2 1996/04/10 12:33:33 mclareni Exp $

      SUBROUTINE ARRADG(ID,NREM,SNR,PT21,PT23)

C...ARiadne subroutine RADiate Gluon

C...Performs the radiation of a gluon from dipole ID

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arhide.f"
#include "ardat1.f"
#include "arint2.f"
#include "arint4.f"
#include "lujets.f"

      INXT(I)=IDO(IP3(I))


C...Boost dipole to its CMS
      CALL ARBOCM(ID)

C...Copy some information about dipole
      BS=ARMAS2(IP1(ID),IP3(ID))
      IF (ABS(BS-SDIP(ID)).GT.(BS+SDIP(ID))*PARA(39).AND.
     $     MSTA(9).GE.2) CALL ARERRM('ARRADG',13,0)

      BW=SQRT(BS)
      B1=BX1(ID)
      B3=BX3(ID)
      QE1=QEX(IP1(ID))
      QE3=QEX(IP3(ID))

C...If parton not extended - no recoil gluon (trivial)
      IF (.NOT.QE1) AEX1(ID)=2.0
      IF (.NOT.QE3) AEX3(ID)=2.0

C...No recoil gluon if reemission
      IF (NREM.EQ.1) AEX1(ID)=2.0
      IF (NREM.EQ.3) AEX3(ID)=2.0

C...If AEX1(3) >= 1 then no recoil gluon
      IF (MSTA(17).EQ.0) THEN
        AEX1(ID)=2.0
        AEX3(ID)=2.0
      ENDIF

C...No recoil gluons if not enough energy left for original parton
      IF (AEX1(ID).LT.1.0.OR.AEX3(ID).LT.1.0) THEN
        BY1=BP(IP1(ID),5)**2/BS
        BY3=BP(IP3(ID),5)**2/BS
        BPT=0.5*BW*(1.0+BY1-BY3+SQRT(1.0+(BY1-BY3)**2-2.0*(BY1+BY3)))
        IF (MSTA(25).GT.0) THEN
          B1P=(1.0-AEX1(ID))*(BPT-BP(IP1(ID),5))+BP(IP1(ID),5)
        ELSE
          B1P=(1.0-AEX1(ID))*BPT
        ENDIF
        IF (B1P.LE.BP(IP1(ID),5)) THEN
          AEX1(ID)=2.0
          B1P=0.0
          B1M=0.0
        ELSE
          B1M=BS*BY1/B1P
        ENDIF
        BMT=0.5*BW*(1.0+BY3-BY1+SQRT(1.0+(BY1-BY3)**2-2.0*(BY1+BY3)))
        IF (MSTA(25).GT.0) THEN
          B3M=(1.0-AEX3(ID))*(BMT-BP(IP3(ID),5))+BP(IP3(ID),5)
        ELSE
          B3M=(1.0-AEX3(ID))*BMT
        ENDIF
        IF (B3M.LE.BP(IP3(ID),5)) THEN
          AEX3(ID)=2.0
          B3P=0.0
          B3M=0.0
        ELSE
          B3P=BS*BY3/B3M
        ENDIF
      ENDIF

C...Check if any parton can take full recoil.
      QR1=(QQ(IP1(ID)).AND.MSTA(16).GE.1.AND.((.NOT.QEX(IP1(ID))).OR.
     $     (QEX(IP1(ID)).AND.MSTA(16).EQ.2.AND.AEX1(ID).GE.1.0)))
      QR3=(QQ(IP3(ID)).AND.MSTA(16).GE.1.AND.((.NOT.QEX(IP3(ID))).OR.
     $     (QEX(IP3(ID)).AND.MSTA(16).EQ.2.AND.AEX3(ID).GE.1.0)))

C...No recoil gluons if one parton can take full recoil
      IF ((AEX1(ID).LT.1.0.OR.AEX3(ID).LT.1.0).AND.
     $     (MSTA(17).EQ.1.OR.MSTA(17).EQ.2)) THEN
        IF (QR3.AND.NREM.NE.3) AEX1(ID)=2.0
        IF (QR1.AND.NREM.NE.1) AEX3(ID)=2.0
      ENDIF

      QRG1=(AEX1(ID).LT.1.0)
      QRG3=(AEX3(ID).LT.1.0)

      IDE=ID

C...Add recoil gluon for parton 1
      IF (QRG1) THEN
        CALL ARADDG(ID,1)
        IDE=INXT(ID)
        BP(IP1(ID),1)=0.0
        BP(IP1(ID),2)=0.0
        BP(IP1(ID),3)=0.5*(B1P-B1M)
        BP(IP1(ID),4)=0.5*(B1P+B1M)
        INO(IP3(ID))=-IO
      ENDIF

C...Add emitted gluon
      IF ((MHAR(134).EQ.0.AND.B1.GT.B3).OR.
     $     (MHAR(134).EQ.1.AND.B1**2.GT.RLU(0)*(B1**2+B3**2))) THEN
        CALL ARADDG(IDE,3)
      ELSE
        CALL ARADDG(IDE,1)
      ENDIF

      INO(IP3(IDE))=IO

C...Add recoil gluon for parton 3
      IF (QRG3) THEN
        IDL=INXT(IDE)
        CALL ARADDG(IDL,3)
        IDL=INXT(IDL)
        BP(IP3(IDL),1)=0.0
        BP(IP3(IDL),2)=0.0
        BP(IP3(IDL),3)=0.5*(B3P-B3M)
        BP(IP3(IDL),4)=0.5*(B3P+B3M)
        INO(IP1(IDL))=-IO
      ENDIF

      IF (NREM.EQ.0) THEN
        IF (QRG1.AND.QRG3) THEN
          SNR3=BS*((BW-B1M)*(1.0-B1+BY1-BY3)/BW+BY3)
          SNR1=BS*((BW-B3P)*(1.0-B3+BY3-BY1)/BW+BY1)
        ELSEIF (QRG1) THEN
          SNR=BS*(1.0-B3+BY3)
        ELSEIF (QRG3) THEN
          SNR=BS*(1.0-B1+BY1)
        ELSE
          SNR=0.0
        ENDIF
      ENDIF

      PT21=0.0
      PT23=0.0
      IF (QRG1.OR.QRG3) THEN
        B2M=(1.0-B3+BY3-BY1)*BW
        B2P=(1.0-B1+BY1-BY3)*BW
        IF (QRG1.AND.MSTA(17).GE.2) PT21=(B2M*B2P**3)/(BW-B1P-B2P)**2
        IF (QRG3.AND.MSTA(17).GE.2) PT23=(B2P*B2M**3)/(BW-B3M-B2M)**2
        DA=(BW-B1P-B3P)/(BW-B1M-B3M)
        SA=(BW-B1P-B3P)*(BW-B1M-B3M)/BS
        DB=(DA-1.0D0)/(DA+1.0D0)
        BY1A=BY1/SA
        IF (QRG1) BY1A=0.0
        BY3A=BY3/SA
        IF (QRG3) BY3A=0.0
        BS=BS*SA
        B1=1.0-(1.0-B1+BY1-BY3)/SQRT(SA*DA)+BY1A-BY3A
        B3=1.0-(1.0-B3+BY3-BY1)/SQRT(SA/DA)+BY3A-BY1A

        IF (QRG1) CALL AROBO1(0.0,0.0,0.0D0,0.0D0,-DB,IP1(ID))
        IF (QRG3) CALL AROBO1(0.0,0.0,0.0D0,0.0D0,-DB,IP3(IDL))
      ENDIF

C...Disable Kleiss orientation if extended partons
      IF (QR1.AND.QR3.AND.(QE1.OR.QE3)) THEN
        QR1=.FALSE.
        QR3=.FALSE.
      ENDIF

C...Orientate the emitted partons
      IF (NREM.EQ.0) THEN
        CALL ARORIE(IP1(IDE),IP3(IDE),IP3(INXT(IDE)),BS,B1,B3,QR1,QR3,
     $       PT21,PT23)
      ELSEIF (NREM.EQ.1) THEN
        QR1=.FALSE.
        QR3=.TRUE.
        CALL ARORIE(IP1(IDE),IP3(IDE),IP3(INXT(IDE)),BS,B1,B3,
     $       QR1,QR3,0.0,0.0)
      ELSEIF (NREM.EQ.3) THEN
        QR1=.TRUE.
        QR3=.FALSE.
        CALL ARORIE(IP1(IDE),IP3(IDE),IP3(INXT(IDE)),BS,B1,B3,
     $       QR1,QR3,0.0,0.0)
      ENDIF

C...Boost created dipoles back to original CMS, Optionally including
      IF ((.NOT.QRG1).AND.(.NOT.QRG3)) THEN
        CALL AROBO3(THE,PHI,DBEX,DBEY,DBEZ,
     $              IP1(IDE),IP3(IDE),IP3(INXT(IDE)))
      ELSEIF (QRG1.AND.(.NOT.QRG3)) THEN
        IF (MSTA(17).LT.2) PT21=ARIPT2(IP1(ID),IP1(IDE),IP3(IDE))
        CALL AROBO4(0.0,0.0,0.0D0,0.0D0,DB,
     $              IP1(ID),IP1(IDE),IP3(IDE),IP3(INXT(IDE)))
        CALL AROBO4(THE,PHI,DBEX,DBEY,DBEZ,
     $              IP1(ID),IP1(IDE),IP3(IDE),IP3(INXT(IDE)))
      ELSEIF ((.NOT.QRG1).AND.QRG3) THEN
        IF (MSTA(17).LT.2)
     $       PT23=ARIPT2(IP1(IDE),IP3(IDE),IP3(INXT(IDE)))
        CALL AROBO4(0.0,0.0,0.0D0,0.0D0,DB,
     $              IP1(IDE),IP3(IDE),IP3(INXT(IDE)),IP3(IDL))
        CALL AROBO4(THE,PHI,DBEX,DBEY,DBEZ,
     $              IP1(IDE),IP3(IDE),IP3(INXT(IDE)),IP3(IDL))
      ELSEIF (QRG1.AND.QRG3) THEN
        IF (MSTA(17).LT.2) THEN
          PT21=ARIPT2(IP1(ID),IP1(IDE),IP3(IDE))
          PT23=ARIPT2(IP3(IDE),IP3(INXT(IDE)),IP3(IDL))
        ENDIF
        IF (PT21.GE.PT23) THEN
          SNR=SNR3
        ELSE
          SNR=SNR1
        ENDIF
        CALL AROBO5(0.0,0.0,0.0D0,0.0D0,DB,
     $              IP1(ID),IP1(IDE),IP3(IDE),IP3(INXT(IDE)),IP3(IDL))
        CALL AROBO5(THE,PHI,DBEX,DBEY,DBEZ,
     $              IP1(ID),IP1(IDE),IP3(IDE),IP3(INXT(IDE)),IP3(IDL))
      ENDIF

C...Special treatment for Drell-Yan produced particles
      IF (QQ(MAXPAR-2).AND.NREM.EQ.0) THEN
        IF (ARDYRE(IDE,BW,QRG1,QRG3).GT.0.0) SNR=0.0
      ENDIF

C...Register radiated gluon for subsequent calculation of azimuthal
C...Asymmetry for O(alpha_) lepto-production  ME
      IG=IP3(IDE)
      IF (IO.EQ.1.AND.MSTA(1).EQ.3.AND.ABS(MSTA(33)).EQ.1) THEN
        DO 200 J=1,5
          BASS(J)=BP(IG,J)
 200    CONTINUE
        IFLASS=IFL(IG)
      ENDIF

      IF (IO.EQ.1.AND.NREM.EQ.0) THEN
        PHAR(121)=0.5*LOG(MAX(BP(IG,4)+BP(IG,3),1.0D-30)/
     $       MAX(BP(IG,4)-BP(IG,3),1.0D-30))
        PHAR(122)=BP(IG,1)**2+BP(IG,2)**2
      ENDIF

 100  CONTINUE

      RETURN

C**** END OF ARRADG ****************************************************
      END
