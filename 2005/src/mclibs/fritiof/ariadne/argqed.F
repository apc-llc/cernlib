*
* $Id: argqed.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: argqed.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: argqed.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE ARGQED(ID)
 
C...ARiadne subroutine Generate pt2 for QED emission
 
C...Generates a p-t^2 for a possible QED emission from dipole ID.
 
 
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
      EXTERNAL ARNDX2,ARNDY1,ARVET1,ARVET2
      REAL ARNDX2,ARNDY1,ARVET1,ARVET2
 
 
C...Copy information about partons in dipole (for explanation see
C...subroutine ARGQCD
      PT2IN(ID)=0.0
      S=SDIP(ID)
      IF(S.LE.4.0*PARA(5)**2) RETURN
      IF(MSTA(20).GE.2.AND.ISTRS.GE.2) RETURN
      W=SQRT(S)
      XT2MP=PT2LST/S
      QQ1=QQ(IP1(ID))
      QQ3=QQ(IP3(ID))
      NE1=IEX(IP1(ID))
      NE3=IEX(IP3(ID))
 
      SY1=BP(IP1(ID),5)/W
      SY2=0.0
      SY3=BP(IP3(ID),5)/W
 
      XT2C=PARA(5)**2/S
      NXP1=2
      NXP3=2
 
C...Set charges of emitting quarks and set constant in cross section
      IQ1=LUCHGE(IFL(IP1(ID)))
      IQ3=LUCHGE(IFL(IP3(ID)))
      FQMAX=FLOAT(MAX(ABS(IQ1),ABS(IQ3)))
      FQ1=FLOAT(IQ1)/FQMAX
      FQ3=FLOAT(IQ3)/FQMAX
      C=(FQMAX**2)/(9.0*PARU(1))
      IFLG=-1
 
C...Set mass dependent parameters
      CALL ARMADE
 
C...Restrict phase space if demanded
      IF(MSTA(11).EQ.0.OR.MSTA(11).EQ.2) XT2M=MIN(XT2M,XT2MP)
 
C...Set some further parameters and call the veto algorithm with
C...suitable random functions for constant alpha_EM.
      YINT=1.0
      CN=2.0/(C*PARA(4))
      CALL ARMCDI(ARNDX2,ARNDY1,ARVET1)
 
C...Save information about emission
      PT2IN(ID)=XT2*S
      BX1(ID)=B1
      BX3(ID)=B3
      AEX1(ID)=AE1
      AEX3(ID)=AE3
 
      RETURN
 
C**** END OF ARGQED ****************************************************
      END
