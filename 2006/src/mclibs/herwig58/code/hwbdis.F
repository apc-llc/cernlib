*
* $Id: hwbdis.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: hwbdis.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWBDIS
*CMZ :-        -17/05/94  09.33.08  by  Mike Seymour
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWBDIS(IOPT)
C     FILL MISSING AREA OF DIS PHASE-SPACE WITH 2+1-JET EVENTS
C     IF (IOPT.EQ.1) SET UP EVENT RECORD
C     IF (IOPT.EQ.2) CLEAN UP EVENT RECORD AFTER SHOWERING
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION P1(5),P2(5),P3(5),PCMF(5),L(5),R(3,3),Q,XBJ,
     $     RN,XPMIN,XPMAX,XP,ZPMIN,ZPMAX,ZP,FAC,X1,X2,XTSQ,XT,PTSQ,
     $     SIN1,SIN2,W1,W2,CFAC,PDFOLD(13),PDFNEW(13),PHI,SCALE,
     $     Q1(5),Q2(5),
     $     COMINT,BGFINT,COMWGT,C1,C2,CM,B1,B2,BM,
     $     HWRGEN,HWBVMC,HWUALF,HWULDO
      LOGICAL BGF
      INTEGER IOPT,EMIT,ICMF,IHEP,JHEP,IIN,IOUT,ILEP,IHAD,ID,IDNEW,
     $     IEDT(3),NDEL
      SAVE BGF,IIN,IOUT,ICMF,ID,Q1,Q2
      DATA EMIT,COMINT,BGFINT,COMWGT/0,3.9827,1.2462,0.3/
      DATA C1,C2,CM,B1,B2,BM/0.56,0.20,10,0.667,0.167,3/
      IF (IERROR.NE.0) RETURN
      IF (IOPT.EQ.1) THEN
C---FIND AN UNTREATED CMF
        IF (EMIT.EQ.NEVHEP+NWGTS) RETURN
        ICMF=0
        DO 10 IHEP=1,NHEP
 10       IF (ICMF.EQ.0 .AND. ISTHEP(IHEP).EQ.110 .AND.
     &    JDAHEP(2,IHEP).EQ.JDAHEP(1,IHEP)+1) ICMF=IHEP
        IF (ICMF.EQ.0) RETURN
        IIN=JMOHEP(2,ICMF)
        IOUT=JDAHEP(2,ICMF)
        ILEP=JMOHEP(1,ICMF)
        CALL HWVEQU(5,PHEP(1,IIN),P1)
        CALL HWVEQU(5,PHEP(1,IOUT),P2)
        CALL HWVEQU(5,PHEP(1,ILEP),L)
        IHAD=2
        IF (JDAHEP(1,IHAD).NE.0) IHAD=JDAHEP(1,IHAD)
        ID=IDHW(IIN)
C---STORE OLD MOMENTA
        CALL HWVEQU(5,P1,Q1)
        CALL HWVEQU(5,P2,Q2)
C---BOOST AND ROTATE THE MOMENTA TO THE BREIT FRAME
        CALL HWVDIF(4,P2,P1,PCMF)
        CALL HWUMAS(PCMF)
        CALL HWVEQU(5,PHEP(1,IHAD),P1)
        Q=-PCMF(5)
        XBJ=HALF*Q**2/HWULDO(P1,PCMF)
        CALL HWVSCA(4,HALF/XBJ,PCMF,PCMF)
        CALL HWVSUM(4,P1,PCMF,PCMF)
        CALL HWUMAS(PCMF)
        CALL HWULOF(PCMF,L,L)
        CALL HWULOF(PCMF,P1,P1)
        CALL HWUROT(P1,ONE,ZERO,R)
        CALL HWUROF(R,L,L)
        PHI=ATAN2(L(2),L(1))
        CALL HWUROT(P1,COS(PHI),SIN(PHI),R)
C---CHOOSE THE HADRONIC-PLANE CONFIGURATION, XP,ZP
        IF (HWRGEN(0).LT.COMWGT) THEN
C-----CONSIDER GENERATING A QCD COMPTON EVENT
          BGF=.FALSE.
          P3(5)=RMASS(13)
 100      RN=HWRGEN(1)
          IF (RN.LT.C1) THEN
            ZP=HWRGEN(2)
            XPMAX=MIN(ZP,1-ZP)
            XP=HWRGEN(3)*XPMAX
            FAC=1/C1*2*XPMAX/((1-XP)*(1-ZP))*
     $           (1+(1-XP-ZP+2*XP*ZP)**2+2*(1-XP)*(1-ZP)*XP*ZP)
            IF (2*HWRGEN(4).LT.1) THEN
              ZPMAX=ZP
              ZP=XP
              XP=ZPMAX
            ENDIF
          ELSEIF (RN.LT.C1+C2) THEN
            XPMAX=0.83
            XP=XPMAX*HWRGEN(2)
            ZPMIN=MAX(XP,1-XP)
            ZPMAX=1-2./3.*XP*(1+REAL( CMPLX(10-45*XP+18*XP**2,3*SQRT(
     $         3*(9+66*XP-93*XP**2+12*XP**3-8*XP**4+24*XP**5-8*XP**6)))
     $         **(1./3.) * CMPLX(0.5,0.8660254) ))
            ZP=1-((1-ZPMIN)/(1-ZPMAX))**HWRGEN(4)*(1-ZPMAX)
            FAC=1/C2*XPMAX*LOG((1-ZPMIN)/(1-ZPMAX))/(1-XP)*
     $           (1+(1-XP-ZP+2*XP*ZP)**2+2*(1-XP)*(1-ZP)*XP*ZP)
          ELSE
            ZPMAX=0.85
            ZP=ZPMAX*HWRGEN(2)
            XPMIN=MAX(ZP,1-ZP)
            XPMAX=(1+4*ZP*(1-ZP))/(1+6*ZP*(1-ZP))
            XP=1-((1-XPMIN)/(1-XPMAX))**HWRGEN(4)*(1-XPMAX)
            FAC=1/(1-C1-C2)*ZPMAX*LOG((1-XPMIN)/(1-XPMAX))/(1-ZP)*
     $           (1+(1-XP-ZP+2*XP*ZP)**2+2*(1-XP)*(1-ZP)*XP*ZP)
          ENDIF
          XPMAX=(1+4*ZP*(1-ZP))/(1+6*ZP*(1-ZP))
          ZPMAX=1-2./3.*XP*(1+REAL( CMPLX(10-45*XP+18*XP**2,3*SQRT(
     $         3*(9+66*XP-93*XP**2+12*XP**3-8*XP**4+24*XP**5-8*XP**6)))
     $         **(1./3.) * CMPLX(0.5,0.8660254) ))
          IF (XP.GT.XPMAX.OR.ZP.GT.ZPMAX.OR.CM*HWRGEN(4).GT.FAC)
     $         GOTO 100
        ELSE
C-----CONSIDER GENERATING A BGF EVENT
          BGF=.TRUE.
          P3(5)=P1(5)
          P1(5)=RMASS(13)
 110      RN=HWRGEN(1)
          IF (RN.LT.B1) THEN
            ZP=HWRGEN(2)
            XPMAX=MIN(ZP,1-ZP)
            XP=HWRGEN(3)*XPMAX
            FAC=1/B1*2*XPMAX/(1-ZP)*
     $           ((  XP+ZP-2*XP*ZP)**2+2*(1-XP)*(1-ZP)*XP*ZP
     $           +(1-XP-ZP+2*XP*ZP)**2+2*(1-XP)*(1-ZP)*XP*ZP)
            IF (2*HWRGEN(4).LT.1) XP=1-XP
          ELSEIF (RN.LT.B1+B2) THEN
            XPMAX=0.83
            XP=XPMAX*HWRGEN(2)
            ZPMIN=MAX(XP,1-XP)
            ZPMAX=1-2./3.*XP*(1+REAL( CMPLX(10-45*XP+18*XP**2,3*SQRT(
     $         3*(9+66*XP-93*XP**2+12*XP**3-8*XP**4+24*XP**5-8*XP**6)))
     $         **(1./3.) * CMPLX(0.5,0.8660254) ))
            ZP=1-((1-ZPMIN)/(1-ZPMAX))**HWRGEN(4)*(1-ZPMAX)
            FAC=1/B2*XPMAX*LOG((1-ZPMIN)/(1-ZPMAX))*
     $           ((  XP+ZP-2*XP*ZP)**2+2*(1-XP)*(1-ZP)*XP*ZP
     $           +(1-XP-ZP+2*XP*ZP)**2+2*(1-XP)*(1-ZP)*XP*ZP)
          ELSE
            XPMAX=0.83
            XP=XPMAX*HWRGEN(2)
            ZPMAX=MIN(XP,1-XP)
            ZPMIN=2./3.*XP*(1+REAL( CMPLX(10-45*XP+18*XP**2,3*SQRT(
     $         3*(9+66*XP-93*XP**2+12*XP**3-8*XP**4+24*XP**5-8*XP**6)))
     $         **(1./3.) * CMPLX(0.5,0.8660254) ))
            ZP=(ZPMAX-ZPMIN)*HWRGEN(4)+ZPMIN
            FAC=1/(1-B1-B2)*XPMAX*(ZPMAX-ZPMIN)/(1-ZP)*
     $           ((  XP+ZP-2*XP*ZP)**2+2*(1-XP)*(1-ZP)*XP*ZP
     $           +(1-XP-ZP+2*XP*ZP)**2+2*(1-XP)*(1-ZP)*XP*ZP)
          ENDIF
          ZPMAX=1-2./3.*XP*(1+REAL( CMPLX(10-45*XP+18*XP**2,3*SQRT(
     $         3*(9+66*XP-93*XP**2+12*XP**3-8*XP**4+24*XP**5-8*XP**6)))
     $         **(1./3.) * CMPLX(0.5,0.8660254) ))
          IF (ZP.GT.ZPMAX.OR.ZP.LT.1-ZPMAX.OR.BM*HWRGEN(4).GT.FAC)
     $         GOTO 110
        ENDIF
        X1=1-   ZP /XP
        X2=1-(1-ZP)/XP
        XTSQ=4*(1-XP)*(1-ZP)*ZP/XP
        XT=SQRT(XTSQ)
        SIN1=XT/SQRT(X1**2+XTSQ)
        SIN2=XT/SQRT(X2**2+XTSQ)
C---CHECK AVAILABLE PHASE-SPACE
        IF (XP.LT.XBJ) RETURN
C---CALCULATE THE ADDITIONAL FACTORS IN THE WEIGHT
        IF (BGF) THEN
          IDNEW=13
          CFAC=1./2
          FAC=BGFINT/(1-COMWGT)
        ELSE
          IDNEW=ID
          CFAC=4./3
          FAC=COMINT/COMWGT
        ENDIF
        SCALE=Q*SQRT(0.25*XTSQ+1)
        CALL HWSFUN(XBJ,Q,IDHW(IHAD),NSTRU,PDFOLD,2)
        CALL HWSFUN(XBJ/XP,SCALE,IDHW(IHAD),NSTRU,PDFNEW,2)
        IF (PDFOLD(ID).LE.0) CALL HWWARN('HWBDIS',100,*999)
        FAC=CFAC/(2*PIFAC) * HWUALF(1,SCALE) * FAC *
     $       PDFNEW(IDNEW)/PDFOLD(ID)
C---DECIDE WHETHER TO MAKE AN EVENT HERE
        IF (HWRGEN(4).GT.FAC) RETURN
C---CHOOSE THE AZIMUTH BETWEEN THE TWO PLANES
        IF (BGF) THEN
          W1=XP**2*(X1**2+1.5*XTSQ)
        ELSE
          W1=1
        ENDIF
        W2=XP**2*(X2**2+1.5*XTSQ)
        IF (HWRGEN(5)*(W1+W2).GT.W2) THEN
          IF (BGF) THEN
C-----WEIGHTED BY (1+SIN1*COS(PHI))**2
 200        PHI=(2*HWRGEN(6)-1)*PIFAC
            IF (HWRGEN(7)*(1+SIN1)**2.GT.(1+SIN1*COS(PHI))**2) GOTO 200
          ELSE
C-----UNIFORMLY
            PHI=(2*HWRGEN(6)-1)*PIFAC
          ENDIF
        ELSE
C-----WEIGHTED BY (1-SIN2*COS(PHI))**2
 210      PHI=(2*HWRGEN(6)-1)*PIFAC
          IF (HWRGEN(7)*(1+SIN2)**2.GT.(1-SIN2*COS(PHI))**2) GOTO 210
        ENDIF
C---RECONSTRUCT MOMENTA AND BOOST BACK TO LAB
        P1(1)=0
        P1(2)=0
        P1(3)=HALF*Q/XP
        P1(4)=SQRT(P1(3)**2+P1(5)**2)
        PTSQ=((ZP*Q*(P1(4)+P1(3)-Q)-P2(5)**2)*(P1(4)-P1(3)+(1-ZP)*Q)
     $       -P3(5)**2*ZP*Q)/(P1(4)-P1(3)+Q)
C---CHECK INFRARED CUTOFF FOR THIS PARTON TYPE
        IF (PTSQ.LT.MAX(HWBVMC(ID),HWBVMC(IDHW(IOUT)))**2) RETURN
        P2(1)=SQRT(PTSQ)*COS(PHI)
        P2(2)=SQRT(PTSQ)*SIN(PHI)
        P2(3)=-0.5*(ZP*Q-(PTSQ+P2(5)**2)/(ZP*Q))
        P2(4)= 0.5*(ZP*Q+(PTSQ+P2(5)**2)/(ZP*Q))
        P3(1)=P1(1)-P2(1)
        P3(2)=P1(2)-P2(2)
        P3(3)=P1(3)-P2(3)-Q
        P3(4)=P1(4)-P2(4)
        CALL HWUROB(R,P1,P1)
        CALL HWUROB(R,P2,P2)
        CALL HWUROB(R,P3,P3)
        CALL HWULOB(PCMF,P1,P1)
        CALL HWULOB(PCMF,P2,P2)
        CALL HWULOB(PCMF,P3,P3)
        NHEP=NHEP+1
        CALL HWVEQU(5,P1,PHEP(1,IIN))
        IF (BGF.AND.ID.GT.6.OR..NOT.BGF.AND.ID.LT.7) THEN
          CALL HWVEQU(5,P2,PHEP(1,IOUT))
          CALL HWVEQU(5,P3,PHEP(1,NHEP))
        ELSE
          CALL HWVEQU(5,P3,PHEP(1,IOUT))
          CALL HWVEQU(5,P2,PHEP(1,NHEP))
        ENDIF
        CALL HWVSUM(4,PHEP(1,ILEP),PHEP(1,IIN),PHEP(1,ICMF))
        CALL HWUMAS(PHEP(1,ICMF))
C---STATUS, ID AND POINTERS
        ISTHEP(NHEP)=114
        IF (BGF) THEN
          IDHW(IIN)=13
          IDHEP(IIN)=IDPDG(13)
          IF (ID.LT.7) THEN
            IDHW(NHEP)=IDHW(IOUT)
            IDHEP(NHEP)=IDHEP(IOUT)
            IDHW(IOUT)=MOD(ID,6)+6
            IDHEP(IOUT)=IDPDG(IDHW(IOUT))
          ELSE
            IDHW(NHEP)=MOD(ID,6)
            IDHEP(NHEP)=IDPDG(IDHW(NHEP))
          ENDIF
        ELSEIF (ID.LT.7) THEN
          IDHW(NHEP)=13
          IDHEP(NHEP)=IDPDG(13)
        ELSE
          IDHW(NHEP)=IDHW(IOUT)
          IDHEP(NHEP)=IDHEP(IOUT)
          IDHW(IOUT)=13
          IDHEP(IOUT)=IDPDG(13)
        ENDIF
        JDAHEP(2,ICMF)=NHEP
        JMOHEP(1,NHEP)=ICMF
C---COLOUR CONNECTIONS
        JDAHEP(2,IIN)=NHEP
        JDAHEP(2,NHEP)=IOUT
        JMOHEP(2,IOUT)=NHEP
        JMOHEP(2,NHEP)=IIN
C---FACTORISATION SCALE
        EMSCA=SCALE
        EMIT=NEVHEP+NWGTS
      ELSEIF (IOPT.EQ.2) THEN
C---MAKE TWO-JET EVENTS LOOK LIKE ONE-JET EVENTS
        IF (EMIT.NE.NEVHEP+NWGTS) RETURN
        IF (.NOT.BGF) THEN
          CALL HWVEQU(5,Q1,PHEP(1,IIN))
          CALL HWVEQU(5,Q2,PHEP(1,IOUT))
          JMOHEP(2,IIN)=IOUT
          JDAHEP(2,IIN)=IOUT
          JMOHEP(2,IOUT)=IIN
          JDAHEP(2,IOUT)=IIN
          JDAHEP(2,ICMF)=IOUT
          IHEP=JDAHEP(1,IOUT)
          JHEP=JDAHEP(1,IOUT+1)
          CALL HWVSUM(4,PHEP(1,IHEP),PHEP(1,JHEP),PHEP(1,IHEP))
          CALL HWUMAS(PHEP(1,IHEP))
          JDAHEP(2,IHEP)=JDAHEP(2,JHEP)
          IEDT(1)=IOUT+1
          IEDT(2)=JHEP
          IEDT(3)=JHEP+1
          NDEL=3
          IF (ISTHEP(JHEP+1).NE.100) NDEL=2
          IHEP=JDAHEP(1,IOUT)
          JMOHEP(1,IHEP)=IOUT
          IF (ISTHEP(IHEP+1).EQ.100) THEN
            JMOHEP(1,IHEP+1)=IOUT
            JMOHEP(2,IHEP+1)=IIN
          ENDIF
          DO 300 JHEP=JDAHEP(1,IHEP),JDAHEP(2,IHEP)
            JMOHEP(1,JHEP)=IHEP
 300      CONTINUE
          IF (IDHW(IOUT).EQ.13) IDHW(IOUT)=IDHW(IOUT+1)
          IDHEP(IOUT)=IDPDG(IDHW(IOUT))
          IDHW(IHEP)=IDHW(IOUT)
          CALL HWUEDT(NDEL,IEDT)
        ELSEIF (ID.LT.7) THEN
          CALL HWVEQU(5,Q1,PHEP(1,IIN))
          CALL HWVEQU(5,Q2,PHEP(1,IOUT+1))
          JMOHEP(2,IIN)=IOUT+1
          JDAHEP(2,IIN)=IOUT+1
          JMOHEP(2,IOUT+1)=IIN
          JDAHEP(2,IOUT+1)=IIN
          JDAHEP(2,ICMF)=IOUT+1
          IHEP=JDAHEP(1,IIN)
          JHEP=JDAHEP(1,IOUT)
          CALL HWVDIF(4,PHEP(1,IHEP),PHEP(1,JHEP),PHEP(1,IHEP))
          CALL HWUMAS(PHEP(1,IHEP))
          CALL HWVDIF(4,PHEP(1,ICMF),PHEP(1,JHEP),PHEP(1,ICMF))
          CALL HWUMAS(PHEP(1,ICMF))
          CALL HWUEMV(JDAHEP(2,JHEP)-JDAHEP(1,JHEP)+1,
     $         JDAHEP(1,JHEP),JDAHEP(2,IHEP))
          JHEP=JDAHEP(1,IOUT)
          JDAHEP(2,IHEP)=JDAHEP(2,JHEP)
          IEDT(1)=IOUT
          IEDT(2)=JHEP
          IEDT(3)=JHEP+1
          NDEL=3
          IF (ISTHEP(JHEP+1).NE.100) NDEL=2
          CALL HWUEDT(NDEL,IEDT)
          IHEP=JDAHEP(1,IIN)
          DO 400 JHEP=JDAHEP(1,IHEP),JDAHEP(2,IHEP)
            JMOHEP(1,JHEP)=IHEP
 400      CONTINUE
          IDHW(IIN)=ID
          IDHEP(IIN)=IDPDG(ID)
          IDHW(IHEP)=ID
        ELSE
          CALL HWVEQU(5,Q1,PHEP(1,IIN))
          CALL HWVEQU(5,Q2,PHEP(1,IOUT))
          JMOHEP(2,IIN)=IOUT
          JDAHEP(2,IIN)=IOUT
          JMOHEP(2,IOUT)=IIN
          JDAHEP(2,IOUT)=IIN
          JDAHEP(2,ICMF)=IOUT
          IHEP=JDAHEP(1,IIN)
          JHEP=JDAHEP(1,IOUT+1)
          CALL HWVDIF(4,PHEP(1,IHEP),PHEP(1,JHEP),PHEP(1,IHEP))
          CALL HWUMAS(PHEP(1,IHEP))
          CALL HWVDIF(4,PHEP(1,ICMF),PHEP(1,JHEP),PHEP(1,ICMF))
          CALL HWUMAS(PHEP(1,ICMF))
          CALL HWUEMV(JDAHEP(2,JHEP)-JDAHEP(1,JHEP)+1,
     $         JDAHEP(1,JHEP),JDAHEP(1,IHEP)-1)
          JHEP=JDAHEP(1,IOUT+1)
          JDAHEP(1,IHEP)=JDAHEP(1,JHEP)
          IEDT(1)=IOUT+1
          IEDT(2)=JHEP
          IEDT(3)=JHEP+1
          NDEL=3
          IF (ISTHEP(JHEP+1).NE.100.OR.JHEP.EQ.NHEP) NDEL=2
          CALL HWUEDT(NDEL,IEDT)
          IHEP=JDAHEP(1,IIN)
          DO 500 JHEP=JDAHEP(1,IHEP),JDAHEP(2,IHEP)
            JMOHEP(1,JHEP)=IHEP
 500      CONTINUE
          IDHW(IIN)=ID
          IDHEP(IIN)=IDPDG(ID)
          IDHW(IHEP)=ID
        ENDIF
        EMIT=0
      ELSE
        CALL HWWARN('HWBDIS',500,*999)
      ENDIF
 999  END
