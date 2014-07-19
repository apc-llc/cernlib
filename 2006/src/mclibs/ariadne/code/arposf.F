C***********************************************************************
C $Id: arposf.F,v 1.2 1996/04/10 12:33:30 mclareni Exp $

      SUBROUTINE ARPOSF(KFT,KFSTR,X,XQ2,XPOM,TPOM,KFTF,KFPR,XFP,XFPOM)

C...ARiadne subroutine POmeron Structure Function

C...Return some information about possible pomeron interaction given
C...a target KFT with a struck parton KFSTR at X and XQ2. XFPOM is x
C...times the part of the KFT structure function for KFSTR which is due
C...to a pomeron. It must be smaller or equal to the total
C...given by XFP. XPOM and TPOM are the momentum fraction and
C...virtuality of the responsible pomeron. KFPR is the flavour of the
C...pomeron remnant and KFTF is the KF code of the outgoing target.


#include "arimpl.f"
#include "ardat1.f"
#include "arhide.f"
#include "arpopa.f"


C...Get normalization
      KFL=ABS(KFSTR)
      IF (KFL.EQ.21) KFL=0
      POW=0.5*PPOW
      DO 100 IFL=0,6
        IF (MSTA(34).EQ.2) THEN
          XA(IFL)=1.0
          NB(IFL)=1
          POW=1.0
        ELSEIF (MSTA(34).EQ.3) THEN
          XA(IFL)=1.0
          NB(IFL)=1
          POW=1.0
        ENDIF
        IF (NB(IFL).LT.0.OR.XA(IFL)+POW+NB(IFL).LT.1.0.OR.
     $       XA(IFL).LT.-1.0.OR.XA(IFL)+POW.LT.-1.0)
     $       CALL ARERRM('ARPOSF',25,0)
        IF (IFL.EQ.0) THEN
          IF (PHAR(102).GE.0.0) CF(IFL)=PHAR(102)
          XNORM=CF(IFL)*ARIPSF(XA(IFL),NB(IFL),0.0)
        ELSE
          XNORM=XNORM+2.0*CF(IFL)*ARIPSF(XA(IFL),NB(IFL),0.0)
        ENDIF
 100  CONTINUE
      IF (MSTA(27).EQ.0) XNORM=1.0

C...Get t-integral an generate t
      XNP=0
      SA=0
      BMIN=PARA(40)
      BMAX=0.0
      DO 110 I=1,3
        IF (CA(I).EQ.0) GOTO 110
        IF (PB(I).LT.0) CALL ARERRM('ARPOSF',25,0)
        SA=SA+CA(I)
        BPB=PB(I)
        BMIN=MIN(BMIN,BPB)
        BMAX=MAX(BMAX,BPB)
        IF (MHAR(117).GT.0.OR.PB(I)*XQ2.GT.LOG(PARA(40))) THEN
          XNP=XNP+CA(I)/PB(I)
        ELSE
          XNP=XNP+CA(I)*(1.0-EXP(-PB(I)*XQ2))/PB(I)
        ENDIF
 110  CONTINUE
      XNP=XNP/TOTSIG
      IF (MHAR(117).GT.0.OR.BMIN*XQ2.GT.LOG(PARA(40))) THEN
 120    TPOM=-LOG(RLU(IDUM))/BMIN
        SUM=0
        DO 140 I=1,3
          IF (PB(I)*TPOM.LE.LOG(PARA(40)))
     $         SUM=SUM+CA(I)*EXP(-PB(I)*TPOM)
 140    CONTINUE
        IF (SUM.LT.RLU(IDUM)*SA*EXP(-BMIN*TPOM)) GOTO 120
      ELSE
 130    TPOM=-LOG(1.0-RLU(IDUM)*(1.0-EXP(-BMIN*XQ2)))/BMIN
        SUM=0
        DO 150 I=1,3
          IF (PB(I)*TPOM.LE.LOG(PARA(40)))
     $         SUM=SUM+CA(I)*EXP(-PB(I)*TPOM)
 150    CONTINUE
        IF (SUM.LT.RLU(IDUM)*SA*EXP(-BMIN*TPOM)) GOTO 130
      ENDIF

C...Calculate pomeron part of structure function
      AP=XA(KFL)+POW
      IF (MSTA(34).EQ.1) THEN
        XFPOM=CF(KFL)*XNP*ARIPSF(AP-1.0,NB(KFL),X)*(X**(POW-1.0))/XNORM
      ELSEIF (MSTA(34).EQ.2) THEN
        XFPOM=CF(KFL)*XNP*(1.0/6.0-1.5*X-(1.5+3.0*LOG(X))*X**2+
     $       (17.0/6.0-LOG(X))*X**3)/XNORM
      ELSEIF (MSTA(34).EQ.3) THEN
        XFPOM=CF(KFL)*XNP*(1.0/6.0-2.5*X-(40.0/3.0+10.0*LOG(X))*X**2+
     $       (40.0/3.0-10.0*LOG(X))*X**3+2.5*X**4-(1.0/6.0)*X**5)/XNORM
      ENDIF

C...Generate Z
      NP=0
      IF (MSTA(34).EQ.2) THEN
        NP=3
      ELSEIF (MSTA(34).EQ.3) THEN
        NP=5
      ENDIF
      IF (ABS(AP).LT.PARA(39)) THEN
 200    Z=X**RLU(0)
        IF (((1.0-X/Z)**NP)*((1.0-Z)**NB(KFL)).LT.RLU(IDUM)) GOTO 200
      ELSE
 210    Z=(1.0-RLU(IDUM)*(1.0-X**AP))**(1.0/AP)
        IF (((1.0-X/Z)**NP)*((1.0-Z)**NB(KFL)).LT.RLU(IDUM)) GOTO 210
      ENDIF
      XPOM=X/Z

C..Set flavours
      KFTF=KFT
      KFPR=-KFSTR
      IF (KFPR.EQ.0) KFPR=21

      RETURN

C**** END OF ARPOSF ****************************************************
      END
C***********************************************************************
C $Id: arposf.F,v 1.2 1996/04/10 12:33:30 mclareni Exp $

      REAL FUNCTION ARIPSF(A,N,X)

C...ARiadne function Integrate part of Pomeron Structure Function

C...Returns the integral from X to 1 of the function Z**A*(1-Z)**N for
C...A>-1 and N>=0 used in calculation of pomeron part of nucleon
C...structure function

#include "arimpl.f"


      FAC=1.0
      SUM=0.0
      NI=N
      AI=A
      ARIPSF=1.0/SQRT(A+1.0)
      IF (N.LT.0.OR.A.LE.-1.0.OR.X.LT.0.0.OR.X.GE.1.0) RETURN

 100  IF (NI.EQ.0) THEN
        SUM=SUM+FAC*(1.0-X**(AI+1.0))/(AI+1.0)
        ARIPSF=SUM
        RETURN
      ELSE
        AI=AI+1
        FAC=FAC/AI
        SUM=SUM-FAC*(X**AI)*((1.0-X)**NI)
        FAC=FAC*NI
        NI=NI-1
        GOTO 100
      ENDIF

C**** END OF ARIPSF ****************************************************
      END
