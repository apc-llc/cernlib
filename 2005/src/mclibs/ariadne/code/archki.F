C***********************************************************************
C $Id: archki.F,v 1.2 1996/04/10 12:33:04 mclareni Exp $

      SUBROUTINE ARCHKI(ID,IOK)

C...ARiadne subroutine CHeck KInematics

C...Checks if the generated emission for dipole ID (or current dipole
C...if ID=0) is kinematically allowed.

#include "arimpl.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arint1.f"
#include "arpart.f"
#include "ardips.f"
#include "ardat2.f"
#include "arhide.f"
      common /xdummy/ xx1,xx3

      IOK=0

      IF (ID.NE.0) THEN
        IFLG=IRAD(ID)
        QE1=QEX(IP1(ID))
        QE3=QEX(IP3(ID))
        QQ1=QQ(IP1(ID))
        QQ3=QQ(IP3(ID))
        S=SDIP(ID)
        W=SQRT(S)
        SY1=BP(IP1(ID),5)/W
        SY3=BP(IP3(ID),5)/W
        SY2=0.0
        XT2GG1=-1.0
        XT2GG3=-1.0
        IF (IFLG.NE.0) THEN
          SY2=PQMAS(ABS(IFLG))/W
          IF (IFLG.GT.0) THEN
            XT2GG3=PT2GG(IP3(ID))/S
          ELSE
            XT2GG1=PT2GG(IP1(ID))/S
          ENDIF
          XT2GG3=MAX(XT2GG3,XT2GG1)
          XT2GG1=XT2GG3
        ENDIF
        XT2MP=PT2LST/S
        SSY=SY1+SY2+SY3
        Y1=SY1**2
        Y2=SY2**2
        Y3=SY3**2
        BZP=0.5*(1.0+Y1-Y3+SQRT(1.0+(Y1-Y3)**2-2.0*(Y1+Y3)))
        BZM=0.5*(1.0+Y3-Y1+SQRT(1.0+(Y1-Y3)**2-2.0*(Y1+Y3)))
        B1=BX1(ID)
        B3=BX3(ID)
        AE1=AEX1(ID)
        AE3=AEX3(ID)
        IF (MSTA(25).GT.0) THEN
          BP1=(1.0-AE1)*(BZP-SY1)+SY1
        ELSE
          BP1=(1.0-AE1)*BZP
        ENDIF
        IF (BP1.LE.SY1) THEN
          BP1=0.0
          BM1=0.0
        ELSE
          BM1=Y1/BP1
        ENDIF
        IF (MSTA(25).GT.0) THEN
          BM3=(1.0-AE3)*(BZM-SY3)+SY3
        ELSE
          BM3=(1.0-AE3)*BZM
        ENDIF
        IF (BM3.LE.SY3) THEN
          BM3=0.0
          BP3=0.0
        ELSE
          BP3=Y3/BM3
        ENDIF
      ENDIF

      QRG=(MSTA(17).NE.0)
      QNG=((MSTA(17).EQ.1.OR.MSTA(17).EQ.2).AND.MSTA(16).GT.0.AND.
     $     ((QQ1.AND.(.NOT.QE1)).OR.(QQ3.AND.(.NOT.QE3))))
      QRG=(QRG.AND.(.NOT.QNG))

      IF (PARA(19).LT.0.0) THEN
        IF (KQ1.GT.0) THEN
          XX1=1.0-BMRP1-BMR1*B1
          SQ2=XT2*S/(1.0-XX1)
          IF (ARSTRA(KF1,KQ1,XX1,-1.0,SQ2).LT.0.0) RETURN
          XX1=1.0-BMRP1-BMR1*BP1
          SQ2=XT2*S/(1.0-XX1)
          IF (ARSTRA(KF1,KQ1,XX1,-1.0,SQ2).LT.0.0) RETURN
        ENDIF
        IF (KQ3.GT.0) THEN
          XX3=1.0-BMRP3-BMR3*B3
          SQ2=XT2*S/(1.0-XX3)
          IF (ARSTRA(KF3,KQ3,XX3,1.0,SQ2).LT.0.0) RETURN
          XX3=1.0-BMRP3-BMR3*BM3
          SQ2=XT2*S/(1.0-XX3)
          IF (ARSTRA(KF3,KQ3,XX3,1.0,SQ2).LT.0.0) RETURN
        ENDIF
      ENDIF

      IF (IFLG.EQ.0.AND.(QE1.OR.QE3).AND.
     $     (BP1.GT.0.0.OR.BM3.GT.0.0).AND.QRG) THEN
        AZ1=1.0-BP1-BP3
        AZ3=1.0-BM1-BM3
        IF (AZ1.LE.0.0) RETURN
        IF (AZ3.LE.0.0) RETURN
        Y1A=Y1/(AZ1*AZ3)
        IF (BP1.GT.0.0) Y1A=0.0
        Y3A=Y3/(AZ1*AZ3)
        IF (BM3.GT.0.0) Y3A=0.0
        BE1=0.5*(1.0-(1.0-B1+Y1-Y3)/AZ1+Y1A-Y3A)
        BE3=0.5*(1.0-(1.0-B3+Y3-Y1)/AZ3+Y3A-Y1A)
        BE2=1.0-BE1-BE3
        BP1A=BE1**2-Y1A
        BP2A=BE2**2-Y2
        BP3A=BE3**2-Y3A
        IF (2.0*(BP1A*BP2A+BP2A*BP3A+BP3A*BP1A).LE.
     $       (BP1A**2+BP2A**2+BP3A**2)) RETURN
        IF (BE1.LT.SQRT(Y1A)) RETURN
        IF (BE2.LT.SY2) RETURN
        IF (BE3.LT.SQRT(Y3A)) RETURN
        PT21=B3
        PT23=B1
        IF (BP1.GT.0.0.AND.BM3.GT.0.0.AND.MSTA(17).GE.2) THEN
          BP2=1.0-B1+Y1-Y3
          BM2=1.0-B3+Y3-Y1
          PT21=(BM2*BP2**3)/(1.0-BP1-BP2)**2
          PT23=(BP2*BM2**3)/(1.0-BM3-BM2)**2
        ENDIF
        IF ((BP1.GT.0.0.AND.BM3.GT.0.0.AND.PT21.GE.PT23).OR.
     $       (BM3.GT.0.0.AND.BP1.LE.0.0)) THEN
          BM3=0.0
          BP3=0.0
        ELSE
          BP1=0.0
          BM1=0.0
        ENDIF
        AZ1=1.0-BP1-BP3
        AZ3=1.0-BM1-BM3
        IF (AZ1.LE.0.0) RETURN
        IF (AZ3.LE.0.0) RETURN
        Y1A=Y1/(AZ1*AZ3)
        IF (BP1.GT.0.0) Y1A=0.0
        Y3A=Y3/(AZ1*AZ3)
        IF (BM3.GT.0.0) Y3A=0.0
        BE1=0.5*(1.0-(1.0-B1+Y1-Y3)/AZ1+Y1A-Y3A)
        BE3=0.5*(1.0-(1.0-B3+Y3-Y1)/AZ3+Y3A-Y1A)
        BE2=1.0-BE1-BE3
        BP1A=BE1**2-Y1A
        BP2A=BE2**2-Y2
        BP3A=BE3**2-Y3A
        IF (2.0*(BP1A*BP2A+BP2A*BP3A+BP3A*BP1A).LE.
     $       (BP1A**2+BP2A**2+BP3A**2)) RETURN
        IF (BE1.LT.SQRT(Y1A)) RETURN
        IF (BE2.LT.SY2) RETURN
        IF (BE3.LT.SQRT(Y3A)) RETURN
      ELSE

        BE1=0.5*B1
        BE3=0.5*B3
        BE2=1.0-BE1-BE3
          
        BP1A=BE1**2-Y1
        BP2A=BE2**2-Y2
        BP3A=BE3**2-Y3
        IF (2.0*(BP1A*BP2A+BP2A*BP3A+BP3A*BP1A).LE.
     $       (BP1A**2+BP2A**2+BP3A**2)) RETURN
        IF (BE1.LT.SY1) RETURN
        IF (BE2.LT.SY2) RETURN
        IF (BE3.LT.SY3) RETURN

        IF (IFLG.NE.0.AND.MSTA(28).NE.0) THEN
          QUITIT=.FALSE.
          SMQQ=1.0D0-B1+Y1
          IF (ABS(MSTA(28)).EQ.2) SMQQ=BC1-B1
          IF (XT2GG1.GT.0.0.AND.XT2GG3.GT.0.0) THEN
            IF (SMQQ.GT.XT2GG1.AND.SMQQ.GT.XT2GG3) QUITIT=.TRUE.
            IF ((SMQQ.GT.XT2GG1.OR.SMQQ.GT.XT2GG3)
     $           .AND.RLU(IDUM).GT.0.5) QUITIT=.TRUE.
          ELSEIF (XT2GG1.GT.0.0) THEN
            IF (SMQQ.GT.XT2GG1) QUITIT=.TRUE.
          ELSEIF (XT2GG3.GT.0.0) THEN
            IF (SMQQ.GT.XT2GG3) QUITIT=.TRUE.
          ENDIF
          IF (QUITIT) RETURN
        ENDIF

      ENDIF

      IF (PHAR(101).GT.0.AND.
     $     ARTPT2(0,S,B1,B3,Y1,Y2,Y3).GT.PHAR(101)) RETURN

      IOK=1

      RETURN

C**** END OF ARCHKI ****************************************************
      END
C***********************************************************************
C $Id: archki.F,v 1.2 1996/04/10 12:33:04 mclareni Exp $

      REAL FUNCTION ARTPT2(ID,SN,BX1IN,BX3IN,Y1IN,Y2IN,Y3IN)

C...Ariadne function True PT2

C...Calculates the (minimum) true p_t^2 of a gluon given x1 and x3 of
C...an emission 


#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
      

      ARTPT2=-1.0

      IF (ID.EQ.0) THEN
        S=SN
        B1=BX1IN
        B3=BX3IN
        Y1=Y1IN
        Y2=Y2IN
        Y3=Y3IN
      ELSE
        S=SDIP(ID)
        B1=BX1(ID)
        B3=BX3(ID)
        Y1=(BP(IP1(ID),5)**2)/SDIP(ID)
        Y2=0.0
        Y3=(BP(IP3(ID),5)**2)/SDIP(ID)
      ENDIF


      BZ12=0.25*B1**2-Y1
      BZ22=0.25*(2.0-B1-B3)**2-Y2
      BZ32=0.25*B3**2-Y3
      BCOS=2.0*BZ12*BZ22+2.0*BZ12*BZ32+2.0*BZ22*BZ32
     $     -BZ12**2-BZ22**2-BZ32**2
      IF (BCOS.LE.0.0) RETURN
      ARTPT2=0.25*S*BCOS/MAX(BZ12,BZ32)

      RETURN

C**** END OF ARTPT2 ****************************************************
      END
