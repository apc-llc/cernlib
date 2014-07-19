*
* $Id: artest.F,v 1.1.1.1 1996/03/08 16:51:04 mclareni Exp $
*
* $Log: artest.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:04  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: artest.F,v 1.1.1.1 1996/03/08 16:51:04 mclareni Exp $
 
      SUBROUTINE ARTEST(IPRINT)
 
C...ARiadne subroutine TEST
 
C...Performs various tests on Ariadne
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARDAT3/ IWRN(40)
      SAVE /ARDAT3/
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
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
      MSTA(9)=1
      MSTA(6)=-1
      MSTA(20)=1
 
      MSTJ(21)=0
 
      CALL ARINIT('ARIADNE')
 
      DO 110 I=1,10000
 
        PARA(1)=0.1+0.5*RLU(IDUM)
        PARA(2)=0.05+0.25*RLU(IDUM)
        PARA(3)=PARA(1)+0.1+RLU(IDUM)
        PARA(5)=0.1+RLU(IDUM)
        PARA(10)=0.5+RLU(IDUM)
        PARA(11)=0.5+RLU(IDUM)
        PARA(12)=5.0+10.0*RLU(IDUM)
        PARA(25)=2.0*RLU(IDUM)
 
        MSTA(11)=INT(5.0*RLU(IDUM))
        MSTA(12)=INT(2.0*RLU(IDUM))
        MSTA(16)=INT(3.0*RLU(IDUM))
        MSTA(17)=INT(4.0*RLU(IDUM))
        MSTA(18)=INT(4.0*RLU(IDUM))
        MSTA(19)=INT(2.0*RLU(IDUM))
        MSTA(25)=INT(3.0*RLU(IDUM))
        MSTA(31)=INT(2.0*RLU(IDUM))
 
        W=10.0*EXP(RLU(IDUM)*LOG(1000.0))
 100    SM1=RLU(IDUM)*20.0
        SM2=RLU(IDUM)*20.0
        E1=0.5*(W**2+SM1**2-SM2**2)/W
        E2=W-E1
        IF (E1.LT.SM1) GOTO 100
        IF (E2.LT.SM2) GOTO 100
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
        K(1,3)=999
        K(1,4)=NE1
        K(1,5)=0
        P(2,1)=0.0
        P(2,2)=0.0
        P(2,3)=SQRT(E2**2-SM2**2)
        P(2,4)=E2
        P(2,5)=SM2
        K(2,1)=1
        K(2,2)=-1
        K(2,3)=999
        K(2,4)=NE2
        K(2,5)=0
 
        CALL AREXEC
        IF (RLU(IDUM).GT.0.99) CALL LUEXEC
 
        IF (IPRINT.GT.0.AND.MOD(I,100).EQ.0) CALL LULIST(2)
 
 110  CONTINUE
 
      NERRA=0
      DO 200 I=1,40
        NERRA=NERRA+IWRN(I)
 200  CONTINUE
 
      NWRNA=IWRN(13)+IWRN(10)
      NERRA=NERRA-NWRNA
      IF (NERRA.EQ.0) THEN
        WRITE(MSTA(7),1000)
      ELSE
        WRITE(MSTA(7),1010) NERRA
      ENDIF
 
      IF (NWRNA.GT.0) WRITE(MSTA(7),1020) NWRNA
 
      NWRNJ=MSTU(27)
      NERRJ=MSTU(23)
 
      IF (NWRNJ+NERRJ.NE.0) WRITE(MSTA(7),1030) NWRNJ,NERRJ
 
 1000 FORMAT('No errors experienced by Ariadne.')
 1010 FORMAT(I5,' errors occurred in Ariadne.')
 1020 FORMAT(I5,' Non-serious warnings issued by Ariadne')
 1030 FORMAT(I5,' warnings and',I5,' errors occured in JETSET when ',
     $     'attempting to fragment',/
     $     ,' parton state produced by Ariadne.')
 
      RETURN
 
C**** END OF ARTEST ****************************************************
      END
