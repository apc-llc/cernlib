*
* $Id: argqcq.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: argqcq.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: argqcq.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      SUBROUTINE ARGQCQ(ID)
 
C...ARiadne subroutine Generate pt2 for QCD emission.
 
C...Generates a p_t^2 for a possible QCD emission from dipole ID
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARDAT2/ PQMAS(10)
      SAVE /ARDAT2/
      COMMON /ARINT1/ BC1,BC3,BZM,BZP,BP1,BM1,BP3,BM3,BPDY,BMDY,
     $                BMRP1,BMR1,BMRP3,BMR3,KQ3,KF3,KQ1,KF1,
     $                B1,B2,B3,XT2,XT,Y,QQ1,QQ3,
     $                QE1,QE3,ALP1,ALP3,XMU1,XMU3,
     $                S,W,C,CN,ALPHA0,XLAM2,IFLG,IFL1,IFL3,
     $                XT2MP,XT2M,XT2C,XTS,XT3,XT1,XT2GG1,XT2GG3,
     $                YINT,YMAX,YMIN,SQ2,YFAC,PTTRUE,
     $                Y1,Y2,Y3,SY1,SY2,SY3,SSY,ZSQEV,
     $                AE1,AE3,NXP1,NXP3,FQ1,FQ3,QFAIL,QEXDY
      SAVE /ARINT1/
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
      EXTERNAL ARNDX1,ARNDX3,ARNDY3,ARNDY4,ARVET5
      REAL ARNDX1,ARNDX3,ARNDY3,ARNDY4,ARVET5
 
 
C...Exit if no point-like gluons
      QG1=((.NOT.QQ1).AND.(.NOT.QE1))
      QG3=((.NOT.QQ3).AND.(.NOT.QE3))
      IF ((.NOT.QG1).AND.(.NOT.QG3)) RETURN
 
C...Colour factors and things in cross section. If g-g dipole
C...q-qbar splitting only calculated for one gluon but double
C...cross section
      C=1.0/(8.0*PARU(1))
      IF (QG1.AND.QG3) C=C*2.0
      SQ2=0.0
 
C...Parton 3 is always assumed to be split
      IF (QG1) THEN
        SY1=SY3
        QE1=QE3
        QE3=.FALSE.
        XMU1=XMU3
        ALP1=ALP3
        XMU3=0.0
        ALP3=0
      ENDIF
 
C...set 'minimum' XT2 to the XT2 of the gluon emission. XT2s below that
C...are not relevant
 
C...Loop over allowed flavours
      DO 100 IFLG=1,MSTA(15)
 
C...Set mass dependent parameters
        SY2=PQMAS(IFLG)/W
        SY3=SY2
        CALL ARMADE
 
C...Set phase space restrictions
        IF (MSTA(11).LT.2.AND.MSTA(28).GE.0) XT2M=MIN(XT2M,XT2MP)
 
C...Exit if not enough energy
        XT2C=MAX(PT2IN(ID),PARA(3)**2)/S
        XT2=0.0
        IF (XT2M.LE.XT2C.OR.SSY.GE.1.0) GOTO 900
 
C...Set additional parameters and call the veto algorith with
C...Suitable random functions
        YINT=2.0*SQRT(S)
C.......Running alpha_QCD
        IF (MSTA(12).GT.0) THEN
          CN=1.0/(YINT*C*ALPHA0)
          IF (QE1.OR.QE3) THEN
C...........extended dipole
            CALL ARMCDI(ARNDX1,ARNDY4,ARVET5)
          ELSE
C...........pointlike dipole
            CALL ARMCDI(ARNDX1,ARNDY3,ARVET5)
          ENDIF
        ELSE
C.........Constant alpha_QCD
          CN=2.0/(YINT*C*PARA(2))
          IF (QE1.OR.QE3) THEN
C...........extended dipole
            CALL ARMCDI(ARNDX3,ARNDY4,ARVET5)
          ELSE
C...........pointlike dipole
            CALL ARMCDI(ARNDX3,ARNDY3,ARVET5)
          ENDIF
        ENDIF
 
C...If Generated XT2 is larger than previous XT2 accept this and save
C...the generated values of p_t^2, x1, x3, a1 and a3
        IF (XT2.GT.XT2C) THEN
          PT2IN(ID)=XT2*S
          BX1(ID)=B1
          BX3(ID)=B3
          AEX1(ID)=AE1
          AEX3(ID)=AE3
          IRAD(ID)=IFLG
        ENDIF
 
 100  CONTINUE
 
C...Exit if gluon emission was chosen
 900  IF (IRAD(ID).EQ.0) RETURN
 
C...Select wich gluon to split
      QFORCE=.FALSE.
      IF (MSTA(28).NE.0) THEN
        SMQQ=1.0-BX1(ID)+Y1
        IF (ABS(MSTA(28)).EQ.2)
     $       SMQQ=SMQQ-4.0*(PQMAS(ABS(IRAD(ID)))/W)**2
        IF (XT2GG1.GT.0.0.AND.(.NOT.QG3).AND.SMQQ.GT.XT2GG1) RETURN
        IF (XT2GG3.GT.0.0.AND.SMQQ.GT.XT2GG3) QFORCE=.TRUE.
      ENDIF
 
      IF ((.NOT.QG3).OR.(QG1.AND.(RLU(IDUM).GT.0.5.OR.QFORCE))) THEN
        IRAD(ID)=-IRAD(ID)
        B1=BX1(ID)
        BX1(ID)=BX3(ID)
        BX3(ID)=B1
        AE1=AEX1(ID)
        AEX1(ID)=AEX3(ID)
        AEX3(ID)=AE1
      ENDIF
 
      RETURN
 
C**** END OF ARGQCQ ****************************************************
      END
