*
* $Id: arradg.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arradg.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arradg.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      SUBROUTINE ARRADG(ID,NREM,SNR,PT21,PT23)
 
C...ARiadne subroutine RADiate Gluon
 
C...Performs the radiation of a gluon from dipole ID
 
 
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
 
 
C...Boost dipole to its CMS
      CALL ARBOCM(ID)
 
C...Copy some information about dipole
      BS=ARMAS2(IP1(ID),IP3(ID))
      IF(ABS(BS-SDIP(ID)).GT.(BS+SDIP(ID))*PARA(39).AND.
     $     MSTA(9).GE.2) CALL ARERRM('ARRADG',13,0)
 
      BW=SQRT(BS)
      B1=BX1(ID)
      B3=BX3(ID)
      NE1=IEX(IP1(ID))
      NE3=IEX(IP3(ID))
 
C...If parton not extended - no recoil gluon (trivial)
      IF(NE1.EQ.0) AEX1(ID)=2.0
      IF(NE3.EQ.0) AEX3(ID)=2.0
 
C...No recoil gluon if reemission
      IF(NREM.EQ.1) AEX1(ID)=2.0
      IF(NREM.EQ.3) AEX3(ID)=2.0
 
C...If AEX1(3) >= 1 then no recoil gluon
      IF(MSTA(17).EQ.0) THEN
        AEX1(ID)=2.0
        AEX3(ID)=2.0
      ENDIF
 
C...No recoil gluons if not enough energy left for original parton
      IF(AEX1(ID).LT.1.0.OR.AEX3(ID).LT.1.0) THEN
        BY1=BP(IP1(ID),5)**2/BS
        BY3=BP(IP3(ID),5)**2/BS
        BPT=0.5*SQRT(BS)*
     $       (1.0+BY1-BY3+SQRT(1.0+(BY1-BY3)**2-2.0*(BY1+BY3)))
        B1P=(1.0-AEX1(ID))*BPT
        IF(B1P.LT.BP(IP1(ID),5)) THEN
          AEX1(ID)=2.0
          B1P=0.0
          B1M=0.0
        ELSE
          B1M=BS*BY1/B1P
        ENDIF
        BMT=0.5*SQRT(BS)*
     $       (1.0+BY3-BY1+SQRT(1.0+(BY1-BY3)**2-2.0*(BY1+BY3)))
        B3M=(1.0-AEX3(ID))*BMT
        IF(B3M.LT.BP(IP3(ID),5)) THEN
          AEX3(ID)=2.0
          B3P=0.0
          B3M=0.0
        ELSE
          B3P=BS*BY3/B3M
        ENDIF
      ENDIF
 
C...Check if any parton can take full recoil.
      QR1=(QQ(IP1(ID)).AND.MSTA(16).GE.1.AND.(IEX(IP1(ID)).EQ.0.OR.
     $     (IEX(IP1(ID)).GE.1.AND.MSTA(16).EQ.2.AND.AEX1(ID).GE.1.0)))
      QR3=(QQ(IP3(ID)).AND.MSTA(16).GE.1.AND.(IEX(IP3(ID)).EQ.0.OR.
     $     (IEX(IP3(ID)).GE.1.AND.MSTA(16).EQ.2.AND.AEX3(ID).GE.1.0)))
 
C...Special treatment for Drell-Yan produced particles
      IF(MSTA(23).GT.0) CALL ARDYRE(ID,*100)
 
C...No recoil gluons if one parton can take full recoil
      IF((AEX1(ID).LT.1.0.OR.AEX3(ID).LT.1.0).AND.MSTA(17).EQ.1) THEN
        IF(QR3) AEX1(ID)=2.0
        IF(QR1) AEX3(ID)=2.0
      ENDIF
 
      QRG1=(AEX1(ID).LT.1.0)
      QRG3=(AEX3(ID).LT.1.0)
 
      IDE=ID
 
C...Add recoil gluon for parton 1
      IF(QRG1) THEN
        CALL ARADDG(ID)
        IDE=INXT(ID)
        BP(IP1(ID),1)=0.0
        BP(IP1(ID),2)=0.0
        BP(IP1(ID),3)=0.5*(B1P-B1M)
        BP(IP1(ID),4)=0.5*(B1P+B1M)
        INO(IP3(ID))=-IO
      ENDIF
 
C...Add emitted gluon
      CALL ARADDG(IDE)
      INO(IP3(IDE))=IO
 
C...Add recoil gluon for parton 3
      IF(QRG3) THEN
        IDL=INXT(IDE)
        CALL ARADDG(IDL)
        IDL=INXT(IDL)
        BP(IP3(IDL),1)=0.0
        BP(IP3(IDL),2)=0.0
        BP(IP3(IDL),3)=0.5*(B3P-B3M)
        BP(IP3(IDL),4)=0.5*(B3P+B3M)
        INO(IP1(IDL))=-IO
      ENDIF
 
      IF(NREM.EQ.0) THEN
        IF(QRG1.AND.QRG3) THEN
          SNR3=BS*((BW-B1M)*(1.0-B1+BY1-BY3)/BW+BY3)
          SNR1=BS*((BW-B3P)*(1.0-B3+BY3-BY1)/BW+BY1)
        ELSEIF(QRG1) THEN
          SNR=BS*(1.0-B3+BY3)
        ELSEIF(QRG3) THEN
          SNR=BS*(1.0-B1+BY1)
        ELSE
          SNR=0.0
        ENDIF
      ENDIF
 
      PT21=0.0
      PT23=0.0
      IF(QRG1.OR.QRG3) THEN
        B2M=(1.0-B3+BY3-BY1)*BW
        B2P=(1.0-B1+BY1-BY3)*BW
        IF(QRG1.AND.MSTA(17).GE.2) PT21=(B2M*B2P**3)/(BW-B1P-B2P)**2
        IF(QRG3.AND.MSTA(17).GE.2) PT23=(B2P*B2M**3)/(BW-B3M-B2M)**2
        DA=(BW-B1P-B3P)/(BW-B1M-B3M)
        SA=(BW-B1P-B3P)*(BW-B1M-B3M)/BS
        DB=(DA-1.0D0)/(DA+1.0D0)
        BY1A=BY1/SA
        IF(QRG1) BY1A=0.0
        BY3A=BY3/SA
        IF(QRG3) BY3A=0.0
        BS=BS*SA
        B1=1.0-(1.0-B1+BY1-BY3)/SQRT(SA*DA)+BY1A-BY3A
        B3=1.0-(1.0-B3+BY3-BY1)/SQRT(SA/DA)+BY3A-BY1A
 
        IF(QRG1) CALL AROBO1(0.0,0.0,0.0D0,0.0D0,-DB,IP1(ID))
        IF(QRG3) CALL AROBO1(0.0,0.0,0.0D0,0.0D0,-DB,IP3(IDL))
      ENDIF
 
C...Disable Kleiss orientation if extended partons
      IF(QR1.AND.QR3.AND.NE1+NE3.NE.0) THEN
        QR1=.FALSE.
        QR3=.FALSE.
      ENDIF
 
C...Orientate the emitted partons
      IF(NREM.EQ.0) THEN
        CALL ARORIE(IP1(IDE),IP3(IDE),IP3(INXT(IDE)),BS,B1,B3,QR1,QR3,
     $       PT21,PT23)
      ELSEIF(NREM.EQ.1) THEN
        QR1=.FALSE.
        QR3=.TRUE.
        CALL ARORIE(IP1(IDE),IP3(IDE),IP3(INXT(IDE)),BS,B1,B3,
     $       QR1,QR3,0.0,0.0)
      ELSEIF(NREM.EQ.3) THEN
        QR1=.TRUE.
        QR3=.FALSE.
        CALL ARORIE(IP1(IDE),IP3(IDE),IP3(INXT(IDE)),BS,B1,B3,
     $       QR1,QR3,0.0,0.0)
      ENDIF
 
C...Boost created dipoles back to original CMS
      IF((.NOT.QRG1).AND.(.NOT.QRG3)) THEN
        CALL AROBO3(THE,PHI,DBEX,DBEY,DBEZ,
     $              IP1(IDE),IP3(IDE),IP3(INXT(IDE)))
      ELSEIF(QRG1.AND.(.NOT.QRG3)) THEN
        IF(MSTA(17).LT.2) PT21=ARIPT2(IP1(ID),IP1(IDE),IP3(IDE))
        CALL AROBO4(0.0,0.0,0.0D0,0.0D0,DB,
     $              IP1(ID),IP1(IDE),IP3(IDE),IP3(INXT(IDE)))
        CALL AROBO4(THE,PHI,DBEX,DBEY,DBEZ,
     $              IP1(ID),IP1(IDE),IP3(IDE),IP3(INXT(IDE)))
      ELSEIF((.NOT.QRG1).AND.QRG3) THEN
        IF(MSTA(17).LT.2) PT23=ARIPT2(IP1(IDE),IP3(IDE),IP3(INXT(IDE)))
        CALL AROBO4(0.0,0.0,0.0D0,0.0D0,DB,
     $              IP1(IDE),IP3(IDE),IP3(INXT(IDE)),IP3(IDL))
        CALL AROBO4(THE,PHI,DBEX,DBEY,DBEZ,
     $              IP1(IDE),IP3(IDE),IP3(INXT(IDE)),IP3(IDL))
      ELSEIF(QRG1.AND.QRG3) THEN
        IF(MSTA(17).LT.2) THEN
          PT21=ARIPT2(IP1(ID),IP1(IDE),IP3(IDE))
          PT23=ARIPT2(IP3(IDE),IP3(INXT(IDE)),IP3(IDL))
        ENDIF
        IF(PT21.GE.PT23) THEN
          SNR=SNR3
        ELSE
          SNR=SNR1
        ENDIF
        CALL AROBO5(0.0,0.0,0.0D0,0.0D0,DB,
     $              IP1(ID),IP1(IDE),IP3(IDE),IP3(INXT(IDE)),IP3(IDL))
        CALL AROBO5(THE,PHI,DBEX,DBEY,DBEZ,
     $              IP1(ID),IP1(IDE),IP3(IDE),IP3(INXT(IDE)),IP3(IDL))
      ENDIF
 
 100  CONTINUE
 
      RETURN
 
C**** END OF ARRADG ****************************************************
      END
