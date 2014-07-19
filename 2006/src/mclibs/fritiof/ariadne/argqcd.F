*
* $Id: argqcd.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: argqcd.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: argqcd.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE ARGQCD(ID)
 
C...ARiadne subroutine Generate pt2 for QCD emission.
 
C...Generates a p_t^2 for a possible QCD emission from dipole ID
 
 
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
 
      COMMON /ARDAT2/ PQMAS(10)
      SAVE /ARDAT2/
 
      COMMON /ARINT1/ BC1,BC3,BZM,BZP,BP1,BM1,BP3,BM3,
     $                B1,B2,B3,XT2,XT,Y,QQ1,QQ3,NE1,NE3,
     $                S,W,C,CN,ALPHA0,XLAM2,IFLG,
     $                XT2MP,XT2ME,XT2M,XT2C,XTS,XT3,XT1,
     $                YINT,YMAX,YMIN,
     $                Y1,Y2,Y3,SY1,SY2,SY3,SSY,
     $                AE1,AE3,NXP1,NXP3,FQ1,FQ3
      SAVE /ARINT1/
 
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
      EXTERNAL ARNDX1,ARNDX2,ARNDX3,ARNDY1,ARNDY2,ARNDY3,ARNDY4,
     $         ARVET3,ARVET4,ARVET5
      REAL ARNDX1,ARNDX2,ARNDX3,ARNDY1,ARNDY2,ARNDY3,ARNDY4,
     $         ARVET3,ARVET4,ARVET5
 
 
C...Copy some information from dipole record
C...S     = the invariant mass squared
C...W     = total energy in dipole
C...XT2MP = maximum allowed fractional p_t^2 (x_t^2) for restricted
C...        phase space option
C...QQ1(3)= Boolean variable 'is quark' for parton 1(3)
C...NE1(3)= integer determining extention of parton 1(3) (0=pointlike)
C...SY1(3)= fractional mass of parton 1(3)
      PT2IN(ID)=0.0
      S=SDIP(ID)
      IF(S.LE.4.0*PARA(3)**2) RETURN
      W=SQRT(S)
      XT2MP=PT2LST/S
      QQ1=QQ(IP1(ID))
      QQ3=QQ(IP3(ID))
      NE1=IEX(IP1(ID))
      NE3=IEX(IP3(ID))
      SY1=BP(IP1(ID),5)/W
      SY3=BP(IP3(ID),5)/W
 
      GOTO 100
 
C...Special entry for checking p_t-ordering of recoil gluons
      ENTRY ARGQTE(ID,SI,XT2MPI,QQ1I,QQ3I,NE1I,NE3I,SY1I,SY3I)
            PT2IN(ID)=0.0
            S=SI
            IF(S.LE.4.0*PARA(3)**2) RETURN
            W=SQRT(S)
            XT2MP=XT2MPI
            QQ1=QQ1I
            QQ3=QQ3I
            NE1=NE1I
            NE3=NE3I
            SY1=SY1I
            SY3=SY3I
 
 100  SY2=0.0
 
C...Calculate maximum x_t^2 for extended dipole
      IF(NE1.GT.0.AND.NE3.EQ.0) XT2ME=((0.25*S*(PARA(10+NE1)**
     $                          PARA(10)))**(2.0/(2.0+PARA(10))))/S
      IF(NE1.EQ.0.AND.NE3.GT.0) XT2ME=((0.25*S*(PARA(10+NE3)**
     $                          PARA(10)))**(2.0/(2.0+PARA(10))))/S
      IF(NE1.GT.0.AND.NE3.GT.0) XT2ME=((0.25*S*((PARA(10+NE1)*
     $           PARA(10+NE3))**PARA(10)))**(1.0/(1.0+PARA(10))))/S
 
C...XLAM = scaled lambda_QCD squared
      XLAM2=PARA(1)**2/S
 
C...C = colour factors etc. in cross section
      C=6.0/(4.0*PARU(1))
      IF(QQ1.AND.QQ3) C=4.0/(3.0*PARU(1))
 
C...alpha_0 for alpha_QCD = alpha_0/ln(p_t^2/lambda_QCD^2)
      ALPHA0=12.0*PARU(1)/(33.0-2.0*MAX(ARNOFL(W,MAX(5,MSTA(15))),3.0))
 
C...Set exponents in cross section
      NXP1=3
      NXP3=3
      IF(QQ1) NXP1=2
      IF(QQ3) NXP3=2
 
C...Flavour of this emission 0 = gluon emission
      IFLG=0
 
C...Minimum x_t^2
      XT2C=PARA(3)**2/S
 
C...Calculate mass dependent parameters
      CALL ARMADE
 
C...Set maximum x_t^2
      IF(MSTA(11).LT.4) XT2M=MIN(XT2M,XT2MP)
      IF(NE1.GT.0.OR.NE3.GT.0) XT2M=MIN(XT2M,XT2ME)
 
      IF(XT2M.LE.XT2C) THEN
        PT2IN(ID)=0.0
        RETURN
      ENDIF
 
C...Set additional parameters and call the veto algorith with
C...Suitable random functions
      IF(MSTA(12).GT.0) THEN
C.......Running alpha_QDC
        YINT=2.0*LOG(0.5/SQRT(XLAM2)+SQRT(0.25/XLAM2-1.0))
        CN=1.0/(YINT*C*ALPHA0)
        IF(NE1.GT.0.OR.NE3.GT.0) THEN
C.........Extended dipole
          CALL ARMCDI(ARNDX1,ARNDY2,ARVET4)
        ELSE
C.........Pointlike dipole
          CALL ARMCDI(ARNDX1,ARNDY1,ARVET4)
        ENDIF
      ELSE
C.......Constant alpha_QCD
        YINT=1.0
        CN=2.0/(C*PARA(2))
        IF(NE1.GT.0.OR.NE3.GT.0) THEN
C.........Extended dipole
          CALL ARMCDI(ARNDX2,ARNDY2,ARVET3)
        ELSE
C.........Pointlike dipole
          CALL ARMCDI(ARNDX2,ARNDY1,ARVET3)
        ENDIF
      ENDIF
 
C...Save the generated values of p_t^2, x1, x3, a1 and a3
      PT2IN(ID)=XT2*S
      BX1(ID)=B1
      BX3(ID)=B3
      AEX1(ID)=AE1
      AEX3(ID)=AE3
      IRAD(ID)=0
 
C...Exit if no q-qbar emission
      IF(MSTA(15).LE.0) RETURN
      QG1=((.NOT.QQ1).AND.NE1.EQ.0)
      QG3=((.NOT.QQ3).AND.NE3.EQ.0)
      IF((.NOT.QG1).AND.(.NOT.QG3)) RETURN
 
C...Colour factors and things in cross section. If g-g dipole
C...q-qbar splitting only calculated forone gluon but double
C...cross section
      C=1.0/(8.0*PARU(1))
      IF(QG1.AND.QG3) C=C*2.0
 
C...Parton 3 is always assumed to be split
      IF(QG1) THEN
        SY1=SY3
        NE1=NE3
        NE3=0
      ENDIF
C...set 'minimum' XT2 to the XT2 of the gluon emission. XT2s below that
C...are not relevant
      XT2C=MAX(XT2,XT2C)
 
C...Loop over allowed flavours
      DO 200 IFLG=1,MSTA(15)
 
C...Set mass dependent parameters
        SY2=PQMAS(IFLG)/W
        SY3=SY2
        CALL ARMADE
 
C...Set phase space restrictions
        IF(MSTA(11).LT.2) XT2M=MIN(XT2M,XT2MP)
        IF(NE1.GT.0.OR.NE3.GT.0) XT2M=MIN(XT2M,XT2ME)
 
C...Exit if not enough energy
        IF(XT2M.LE.XT2C.OR.SSY.GE.1.0) GOTO 300
 
C...Set additional parameters and call the veto algorith with
C...Suitable random functions
        YINT=2.0*SQRT(S)
C.......Running alpha_QCD
        IF(MSTA(12).GT.0) THEN
          CN=1.0/(YINT*C*ALPHA0)
          IF(NE1.GT.0.OR.NE3.GT.0) THEN
C...........pointlike dipole
            CALL ARMCDI(ARNDX1,ARNDY4,ARVET5)
          ELSE
C...........extended dipole
            CALL ARMCDI(ARNDX1,ARNDY3,ARVET5)
          ENDIF
        ELSE
C.........Constant alpha_QCD
          CN=2.0/(YINT*C*PARA(2))
          CN=1.0/(YINT*C*ALPHA0)
          IF(NE1.GT.0.OR.NE3.GT.0) THEN
C...........pointlike dipole
            CALL ARMCDI(ARNDX3,ARNDY4,ARVET5)
          ELSE
C...........extended dipole
            CALL ARMCDI(ARNDX3,ARNDY3,ARVET5)
          ENDIF
        ENDIF
 
C...If Generated XT2 is larger than previous XT2 accept this and save
C...the generated values of p_t^2, x1, x3, a1 and a3
        IF(XT2.GT.XT2C) THEN
          PT2IN(ID)=XT2*S
          BX1(ID)=B1
          BX3(ID)=B3
          AEX1(ID)=AE1
          AEX3(ID)=AE3
          IRAD(ID)=IFLG
          XT2C=XT2
        ENDIF
 
 200  CONTINUE
 
C...Exit if gluon emission was chosen
 300  IF(IRAD(ID).EQ.0) RETURN
 
C...Select wich gluon to split
      IF((.NOT.QG3).OR.(QG1.AND.RLU(IDUM).GT.0.5)) THEN
        IRAD(ID)=-IRAD(ID)
        B1=BX1(ID)
        BX1(ID)=BX3(ID)
        BX3(ID)=B1
        AE1=AEX1(ID)
        AEX1(ID)=AEX3(ID)
        AEX3(ID)=AE1
      ENDIF
 
      RETURN
 
C**** END OF ARGQCD ****************************************************
      END
