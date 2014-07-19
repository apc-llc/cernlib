*
* $Id: artest.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: artest.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: artest.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      SUBROUTINE ARTEST(IPRINT)
 
C...ARiadne subroutine TEST
 
C...Performs various tests on Ariadne
 
 
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
 
      COMMON /ARDAT3/ IWRN(40)
      SAVE /ARDAT3/
 
      COMMON /ARINT1/ BC1,BC3,BZM,BZP,BP1,BM1,BP3,BM3,
     $                B1,B2,B3,XT2,XT,Y,QQ1,QQ3,NE1,NE3,
     $                S,W,C,CN,ALPHA0,XLAM2,IFLG,
     $                XT2MP,XT2ME,XT2M,XT2C,XTS,XT3,XT1,
     $                YINT,YMAX,YMIN,
     $                Y1,Y2,Y3,SY1,SY2,SY3,SSY,
     $                AE1,AE3,NXP1,NXP3,FQ1,FQ3
      SAVE /ARINT1/
 
      COMMON /ARINT2/ DBEX,DBEY,DBEZ,PHI,THE
      SAVE /ARINT2/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
      MSTA(9)=1
      MSTA(6)=-1
      MSTA(20)=1
 
      MSTJ(21)=0
 
      PARA(12)=2.0
      PARA(13)=10.0
 
      CALL ARINIT('ARIADNE')
 
      DO 110 I=1,10000
 
        W=10.0*EXP(RLU(IDUM)*LOG(1000.0))
 100    SM1=RLU(IDUM)*20.0
        SM2=RLU(IDUM)*20.0
        E1=0.5*(W**2+SM1**2-SM2**2)/W
        E2=W-E1
        IF(E1.LT.SM1) GOTO 100
        IF(E2.LT.SM2) GOTO 100
        NE1=INT(RLU(IDUM)*4.0)
        NE2=INT(RLU(IDUM)*4.0)
        N=2
        P(1,1)=0.0
        P(1,2)=0.0
        P(1,3)=-SQRT(E1**2-SM1**2)
        P(1,4)=E1
        P(1,5)=SM1
        K(1,1)=2
        K(1,2)=1
        K(1,3)=0
        K(1,4)=NE1
        K(1,5)=0
        P(2,1)=0.0
        P(2,2)=0.0
        P(2,3)=SQRT(E2**2-SM2**2)
        P(2,4)=E2
        P(2,5)=SM2
        K(2,1)=1
        K(2,2)=-1
        K(2,3)=0
        K(2,4)=NE2
        K(2,5)=0
 
        CALL AREXEC
        CALL LUEXEC
 
        IF(IPRINT.GT.0.AND.MOD(I,100).EQ.0) CALL LULIST(2)
 
 110  CONTINUE
 
      NERRA=0
      DO 200 I=1,40
        NERRA=NERRA+IWRN(I)
 200  CONTINUE
 
      NWRNA=IWRN(13)+IWRN(10)
      NERRA=NERRA-NWRNA
      IF(NERRA.EQ.0) THEN
        WRITE(MSTA(7),1000)
      ELSE
        WRITE(MSTA(7),1010) NERRA
      ENDIF
 
      IF(NWRNA.GT.0) WRITE(MSTA(7),1020) NWRNA
 
      NWRNJ=MSTU(27)
      NERRJ=MSTU(23)
 
      IF(NWRNJ+NERRJ.NE.0) WRITE(MSTA(7),1030) NWRNJ,NERRJ
 
 1000 FORMAT('No errors experienced by Ariadne.')
 1010 FORMAT(I5,' errors occurred in Ariadne.')
 1020 FORMAT(I5,' Non-serious warnings issued by Ariadne')
 1030 FORMAT(I5,' warnings and',I5,' errors occured in JETSET when ',
     $     'attempting to fragment',/
     $     ,' parton state produced by Ariadne.')
 
      RETURN
 
C**** END OF ARTEST ****************************************************
      END
