
C **********************************************************************

      SUBROUTINE LEPTOX

C...Select process and choose kinematical variables (x,y; x,Q2; x,W2)
C...according to the differential cross section.

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      COMMON /FLINFO/ RFLQ,RFLG,RFLM,RFLT
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      DIMENSION PQH(17,2),PNT(2,2),XPQ(-6:6)
      DOUBLE PRECISION DARI27,DARI28
      DATA DARI27,DARI28/2*0.D0/
      DATA W2LOW,W2UPP,YLOW,YUPP,Q2LOW,Q2UPP/6*0./

      DO 10 IH=1,2
      DO 5 I=1,2
    5 PNT(I,IH)=0.
      DO 6 I=1,8
      EWQC(1,IH,I)=0.
    6 EWQC(2,IH,I)=0.
      DO 10 I=1,17
   10 PQH(I,IH)=0.
      DO 20 I=1,17
   20 PQ(I)=0.

      LST(21)=0
      NCUT=0
      S=PARL(21)
      PM2=PSAVE(3,2,5)**2
      IF(LST(2).NE.1) THEN
       Q2LOW=MAX(Q2MIN,X*YMIN*S,(W2MIN-PM2)*X/MAX(1.-X,1.E-22))
       Q2UPP=MIN(Q2MAX,X*YMAX*S,(W2MAX-PM2)*X/MAX(1.-X,1.E-22))
       YLOW=MAX(YMIN,Q2MIN/MAX(S*X,1.E-22),
     & (W2MIN-PM2)/MAX(S*(1.-X),1.E-22))
       YUPP=MIN(YMAX,Q2MAX/MAX(S*X,1.E-22),
     & (W2MAX-PM2)/MAX(S*(1.-X),1.E-22))
       W2LOW=MAX(W2MIN,(1.-X)*YMIN*S+PM2,Q2MIN*(1.-X)/MAX(X,1.E-22)+PM2)
       W2UPP=MIN(W2MAX,(1.-X)*YMAX*S+PM2,Q2MAX*(1.-X)/MAX(X,1.E-22)+PM2)
       GOTO 110
      ENDIF

      IF(PARI(28).LT.0.5) THEN
C...For first call, reset double precision counters.
        DARI27=0.D0
        DARI28=0.D0
      ENDIF
  100 DARI28=DARI28+1.D0
      PARI(28)=DARI28
  101 CONTINUE
C...Choose x according to the distribution
C...hx(x) =  a + b/x + c/x**2 + d/x**3. In detail
C...hq=OPTX(1)/(XMAX-XMIN) + 1/ln(XMAX/XMIN)*OPTX(2)/X
C...   +XMIN*XMAX/(XMAX-XMIN)*OPTX(3)/X**2
C...   +2*(XMIN*XMAX)**2/(XMAX**2-XMIN**2)*OPTX(4)/X**3
      WHICH=(OPTX(1)+OPTX(2)+OPTX(3)+OPTX(4))*RLU(0)
      IF(WHICH.LE.OPTX(1)) THEN
        X=XMIN+RLU(0)*(XMAX-XMIN)
      ELSEIF(WHICH.LE.(OPTX(1)+OPTX(2))) THEN
        X=XMIN*(XMAX/XMIN)**RLU(0)
      ELSEIF(WHICH.LE.(OPTX(1)+OPTX(2)+OPTX(3))) THEN
        X=XMIN*XMAX/(XMAX+RLU(0)*(XMIN-XMAX))
      ELSE
        X=SQRT((XMIN*XMAX)**2/(XMAX**2+RLU(0)*(XMIN**2-XMAX**2)))
      ENDIF
      IF(LST(31).EQ.1) THEN
C...Choose Q**2 according to the distribution
C...hq(Q2) =  a + b/(Q2) + c/(Q2)**2 + d/(Q2)**3. In detail
C...hq=OPTQ2(1)/(Q2MAX-Q2MIN) + 1/ln(Q2MAX/Q2MIN)*OPTQ2(2)/Q2
C...   +Q2MIN*Q2MAX/(Q2MAX-Q2MIN)*OPTQ2(3)/Q2**2
C...   +2*(Q2MIN*Q2MAX)**2/(Q2MAX**2-Q2MIN**2)*OPTQ2(4)/Q2**3
        Q2LOW=MAX(Q2MIN,X*YMIN*S,(W2MIN-PM2)*X/(1.-X))
        Q2UPP=MIN(Q2MAX,X*YMAX*S,(W2MAX-PM2)*X/(1.-X))
        IF(Q2UPP.LT.Q2LOW) GOTO 101
        WHICH=(OPTQ2(1)+OPTQ2(2)+OPTQ2(3)+OPTQ2(4))*RLU(0)
        IF(WHICH.LE.OPTQ2(1)) THEN
          Q2=Q2LOW+RLU(0)*(Q2UPP-Q2LOW)
        ELSEIF(WHICH.LE.(OPTQ2(1)+OPTQ2(2))) THEN
          Q2=Q2LOW*(Q2UPP/Q2LOW)**RLU(0)
        ELSEIF(WHICH.LE.(OPTQ2(1)+OPTQ2(2)+OPTQ2(3))) THEN
          Q2=Q2LOW*Q2UPP/(Q2UPP+RLU(0)*(Q2LOW-Q2UPP))
        ELSE
         Q2=SQRT((Q2LOW*Q2UPP)**2/(Q2UPP**2+RLU(0)*(Q2LOW**2-Q2UPP**2)))
        ENDIF
        Y=Q2/(PARL(21)*X)
        IF(Y.LT.YMIN.OR.Y.GT.YMAX) GOTO 100
      ELSEIF(LST(31).EQ.2) THEN
C...Choose y according to the distribution
C...hy(y) =  a + b/y + c/y**2 + d/y**3. In detail
C...hy=OPTY(1)/(YMAX-YMIN) + 1/ln(YMAX/YMIN)*OPTY(2)/Y
C...   +YMIN*YMAX/(YMAX-YMIN)*OPTY(3)/Y**2
C...   +2*(YMIN*YMAX)**2/(YMAX**2-YMIN**2)*OPTY(4)/Y**3
        YLOW=MAX(YMIN,Q2MIN/(S*X),(W2MIN-PM2)/(S*(1.-X)))
        YUPP=MIN(YMAX,Q2MAX/(S*X),(W2MAX-PM2)/(S*(1.-X)))
        IF(YUPP.LT.YLOW) GOTO 101
        WHICH=(OPTY(1)+OPTY(2)+OPTY(3)+OPTY(4))*RLU(0)
        IF(WHICH.LE.OPTY(1)) THEN
          Y=YLOW+RLU(0)*(YUPP-YLOW)
        ELSEIF(WHICH.LE.(OPTY(1)+OPTY(2))) THEN
          Y=YLOW*(YUPP/YLOW)**RLU(0)
        ELSEIF(WHICH.LE.(OPTY(1)+OPTY(2)+OPTY(3))) THEN
          Y=YLOW*YUPP/(YUPP+RLU(0)*(YUPP-YLOW))
        ELSE
          Y=SQRT((YLOW*YUPP)**2/(YUPP**2+RLU(0)*(YLOW**2-YUPP**2)))
        ENDIF
        Q2=X*Y*PARL(21)
        IF(Q2.LT.Q2MIN.OR.Q2.GT.Q2MAX) GOTO 100
      ELSEIF(LST(31).EQ.3) THEN
C...Choose W**2 according to the distribution
C...hw(W2) =  a + b/(W2) + c/(W2)**2 + d/(W2)**3. In detail
C...hw=OPTW2(1)/(W2MAX-W2MIN) + 1/ln(W2MAX/W2MIN)*OPTW2(2)/W2
C...   +W2MIN*W2MAX/(W2MAX-W2MIN)*OPTW2(3)/W2**2
C...   +2*(W2MIN*W2MAX)**2/(W2MAX**2-W2MIN**2)*OPTW2(4)/W2**3
        W2LOW=MAX(W2MIN,(1.-X)*YMIN*S+PM2,Q2MIN*(1.-X)/X+PM2)
        W2UPP=MIN(W2MAX,(1.-X)*YMAX*S+PM2,Q2MAX*(1.-X)/X+PM2)
        IF(W2UPP.LT.W2LOW) GOTO 101
        WHICH=(OPTW2(1)+OPTW2(2)+OPTW2(3)+OPTW2(4))*RLU(0)
        IF(WHICH.LE.OPTW2(1)) THEN
          W2=W2LOW+RLU(0)*(W2UPP-W2LOW)
        ELSEIF(WHICH.LE.(OPTW2(1)+OPTW2(2))) THEN
          W2=W2LOW*(W2UPP/W2LOW)**RLU(0)
        ELSEIF(WHICH.LE.(OPTW2(1)+OPTW2(2)+OPTW2(3))) THEN
          W2=W2LOW*W2UPP/(W2UPP+RLU(0)*(W2LOW-W2UPP))
        ELSE
         W2=SQRT((W2LOW*W2UPP)**2/(W2UPP**2+RLU(0)*(W2LOW**2-W2UPP**2)))
        ENDIF
        Y=(W2-PM2)/((1.-X)*PARL(21))
        Q2=X*Y*PARL(21)
        IF(Y.LT.YMIN.OR.Y.GT.YMAX) GOTO 100
        IF(Q2.LT.Q2MIN.OR.Q2.GT.Q2MAX) GOTO 100
      ENDIF
  110 IF(LKINEM(LST(2)).NE.0) THEN
        NCUT=NCUT+1
        IF(LST(2).EQ.1) THEN
          IF(NCUT.LE.9999) GOTO 100
          IF(LST(3).GE.1) WRITE(6,1200)
        ENDIF
        LST(21)=2
        RETURN
      ENDIF

      PARI(24)=(1.+(1.-Y)**2)/2.
      PARI(25)=1.-Y
      PARI(26)=(1.-(1.-Y)**2)/2.
      CALL LNSTRF(X,Q2,XPQ)
C...Lepton helicity state, only one contributes in some cases.
      IH=1
      IF(PARL(6).GT.+0.99) IH=2
  200 LST(30)=SIGN(1.,IH-1.5)
      PQH(17,IH)=0.
      PNT(1,IH)=0.
      PNT(2,IH)=0.
      IF(LST(23).EQ.2) THEN
C...Charged current: zero cross-section for one helicity state.
        IF(KSAVE(1).LT.0.AND.IH.EQ.1
     &  .OR.KSAVE(1).GT.0.AND.IH.EQ.2) GOTO 240
        YQ=PARI(24)-LST(30)*PARI(26)
        YQB=PARI(24)+LST(30)*PARI(26)
        IF(PARI(11).GT.1.E-06) THEN
          IF(K(3,2).LT.0) THEN
            PNT(1,IH)=(1.-PARI(11))*PARI(13)*YQ
            PNT(2,IH)=PARI(11)*PARI(12)*YQ
          ELSE
            PNT(1,IH)=(1.-PARI(11))*PARI(12)*YQ
            PNT(2,IH)=PARI(11)*PARI(13)*YQ
          ENDIF
        ENDIF
        DO 220 I=1,LST(12)
        IF(K(3,2)*QC(I).LT.0) THEN
          PQH(I,IH)=XPQ(I)*YQ
        ELSE
          PQH(I+LST(12),IH)=XPQ(-I)*YQB
        ENDIF
  220   CONTINUE
      ELSE
C...Neutral current: electromagnetic or weak or both with interference.
        GFQ2=Q2/(PMAS(23,1)**2+Q2)*SQRT(2.)*PARL(17)*PMAS(23,1)**2/
     &  (3.1415927*PARL(16))
C...Correction to obtain Q**2 dependent alpha-em, if desired.
        AEMCOR=1.
        IF(LST(18).GE.2) AEMCOR=ULALEM(Q2)/PARL(16)
        II=3-IH
        ZLEP=ZL(IH,ILEP+2*INU)
        DO 230 I=1,MAX(LST(12),LST(13))
        A=(-IG*QC(I)*AEMCOR+IZ*GFQ2*ZLEP*ZQ(IH,I))**2
        B=(-IG*QC(I)*AEMCOR+IZ*GFQ2*ZLEP*ZQ(II,I))**2
C...Save helicity-dependent electroweak quark couplings for later use.
        EWQC(1,IH,I)=A
        EWQC(2,IH,I)=B
        IF(I.GT.LST(12)) GOTO 230
        FYQ=(A+B)*PARI(24)+(A-B)*PARI(26)
        PQH(I,IH)=XPQ(I)*FYQ
        IF(I.LE.2.AND.PARI(11).GT.1.E-06) THEN
          PNT(1,IH)=PNT(1,IH)+(1.-PARI(11))*PARI(11+I)*FYQ
          PNT(2,IH)=PNT(2,IH)+PARI(11)*PARI(14-I)*FYQ
        ENDIF
        PQH(I+LST(12),IH)=XPQ(-I)*((A+B)*PARI(24)-(A-B)*PARI(26))
  230   CONTINUE
      ENDIF
  240 CONTINUE
      DO 300 I=1,LST(12)
  300 PQH(17,IH)=PQH(17,IH)+PQH(I,IH)+PQH(I+LST(12),IH)

      IF(ABS(PARL(6)).LT.0.99.AND.IH.EQ.1) THEN
        IH=2
        GOTO 200
      ENDIF

      FLQ=0.
      FLG=0.
      FLM=0.
      FLT=0.
      IF(LST(11).NE.0.AND.(LST(23).EQ.1.OR.LST(23).EQ.4)
     &.AND.LST(2).NE.-3) THEN
C...Include F_L for photon exchange (unless QCD grid being set up)
        LQCD=MOD(LST(11),10)
        LTM=MOD(LST(11)/10,10)
        LHT=LST(11)/100
C...Include QCD, target mass and/or higher twist contr. to long. str fcn
C...FL from interpolation.
        IF(LQCD.EQ.1.OR.LTM.EQ.1) CALL FLIPOL(FLQ,FLG,FLM)
C...Event simulation: if requested, get FL by event-by-event integration
        IF(LST(2).GT.0.AND.
     &  (LQCD.EQ.2.OR.LTM.EQ.2)) CALL FLINTG(FLQ,FLG,FLM)
        IF(LTM.GE.1.OR.LHT.GE.1) THEN
          F2EM=0.
          DO 301 I=1,LST(12)
  301     F2EM=F2EM+QC(I)**2*(XPQ(I)+XPQ(-I))
          IF(LTM.GE.1) FLM=FLM-2.*X**2*PSAVE(3,2,5)**2/Q2*F2EM
          IF(LHT.GE.1) FLT=8.*PARL(19)/Q2*F2EM
        ENDIF
        DO 305 IH=1,2
        PQH17=PQH(17,IH)
C...Note factor 2 at the end, since PQH(IH,17) contains overall factor 2
        PQH(17,IH)=PQH(17,IH)-Y**2*(FLQ+FLG+FLM+FLT)
        DO 305 I=1,16
  305   PQH(I,IH)=PQH(I,IH)*PQH(17,IH)/PQH17
      ENDIF

      DO 310 I=1,17
  310 PQ(I)=(1.-PARL(6))/2.*PQH(I,1)+(1.+PARL(6))/2.*PQH(I,2)

C...Relative contribution from longitudinal str. fcn. and higher twist.
       RFLQ=-Y**2*FLQ/MAX(PQ(17),1.E-33)
       RFLG=-Y**2*FLG/MAX(PQ(17),1.E-33)
       RFLM=-Y**2*FLM/MAX(PQ(17),1.E-33)
       RFLT=-Y**2*FLT/MAX(PQ(17),1.E-33)

C...Common factor for matrix elements.
      IF(LST(31).EQ.1) THEN
        IF(LST(23).EQ.2) THEN
          COMFAC=1./X/(1.+Q2/PMAS(24,1)**2)**2
        ELSE
          COMFAC=1./X/Q2**2
        ENDIF
      ELSEIF(LST(31).EQ.2) THEN
        IF(LST(23).EQ.2) THEN
          COMFAC=1./(1.+Q2/PMAS(24,1)**2)**2*PARL(21)
        ELSE
          COMFAC=1./Q2**2*PARL(21)
        ENDIF
      ELSEIF(LST(31).EQ.3) THEN
        IF(LST(23).EQ.2) THEN
          COMFAC=1./X/(1.+Q2/PMAS(24,1)**2)**2  * X/(1.-X)
        ELSE
          COMFAC=1./X/Q2**2 * X/(1.-X)
        ENDIF
      ENDIF
C-check: Move change of COMFAC to below??
C...Prepare for Q2 weighting.
C     WEIGHT=1/Q2**2
      WEIGHT=1.D0
      COMFAC=COMFAC/WEIGHT
      IF(LST(2).LE.-2) RETURN
      HX=OPTX(1)/(XMAX-XMIN) + 1./ALOG(XMAX/XMIN)*OPTX(2)/X
     &+XMIN*XMAX/(XMAX-XMIN)*OPTX(3)/X**2
     &+2*(XMIN*XMAX)**2/(XMAX**2-XMIN**2)*OPTX(4)/X**3
      XFACT=OPTX(1)+OPTX(2)+OPTX(3)+OPTX(4)
      IF(LST(31).EQ.1) THEN
        HQ2=OPTQ2(1)/(Q2UPP-Q2LOW)
     &  +1./ALOG(Q2UPP/Q2LOW)*OPTQ2(2)/Q2
     &  +Q2LOW*Q2UPP/(Q2UPP-Q2LOW)*OPTQ2(3)/Q2**2
     &  +2*(Q2LOW*Q2UPP)**2/(Q2UPP**2-Q2LOW**2)*OPTQ2(4)/Q2**3
        Q2FACT=OPTQ2(1)+OPTQ2(2)+OPTQ2(3)+OPTQ2(4)
        COMFAC=COMFAC*XFACT*Q2FACT/HX/HQ2
      ELSEIF(LST(31).EQ.2) THEN
        HY=OPTY(1)/(YUPP-YLOW)+1./ALOG(YUPP/YLOW)*OPTY(2)/Y
     &  +YLOW*YUPP/(YUPP-YLOW)*OPTY(3)/Y**2
     &  +2*(YLOW*YUPP)**2/(YUPP**2-YLOW**2)*OPTY(4)/Y**3
        YFACT=OPTY(1)+OPTY(2)+OPTY(3)+OPTY(4)
        COMFAC=COMFAC*XFACT*YFACT/HX/HY
      ELSEIF(LST(31).EQ.3) THEN
        HW2=OPTW2(1)/(W2UPP-W2LOW)
     &  +1./ALOG(W2UPP/W2LOW)*OPTW2(2)/W2
     &  +W2LOW*W2UPP/(W2UPP-W2LOW)*OPTW2(3)/W2**2
     &  +2*(W2LOW*W2UPP)**2/(W2UPP**2-W2LOW**2)*OPTW2(4)/W2**3
        W2FACT=OPTW2(1)+OPTW2(2)+OPTW2(3)+OPTW2(4)
        COMFAC=COMFAC*XFACT*W2FACT/HX/HW2
      ENDIF
      IF(LST(2).LE.0) RETURN

C-check: Move change of COMFAC to here?
      SIGL=(1.-PARL(6))/2.*PQH(17,1)
      SIGR=(1.+PARL(6))/2.*PQH(17,2)
      SIGMA=SIGL+SIGR
      IF(LST(2).EQ.1) THEN
C...When chosing (x,y), reject according to maximum of "cross-section",
C...update cross-section estimate.
        DARI27=DARI27+DBLE(SIGMA)*DBLE(COMFAC)*WEIGHT
        PARI(27)=DARI27
        VIOL=SIGMA*COMFAC/PARI(LST(23))
        IF(VIOL.GT.PARI(32)) THEN
          PARI(32)=VIOL
          IF(PARI(32).GT.1.) THEN
            PARI(LST(23))=PARI(LST(23))*PARI(32)
            IF(LST(3).GE.1) WRITE(6,1300) PARI(32),INT(PARI(30)+1),
     &      PARI(LST(23)),X,Y,Q2,W2
            PARI(32)=1.
          ENDIF
        ENDIF
        IF(VIOL.LT.RLU(0)) GOTO 100
        PARL(24)=PARI(31)*DARI27/DARI28
      ENDIF

      IF(ABS(PARL(6)).LT.0.99) THEN
C...Choose helicity of incoming lepton.
        IH=1
        IF(RLU(0)*SIGMA.GT.SIGL) IH=2
      ENDIF
      LST(30)=SIGN(1.,IH-1.5)

C...Choose target nucleon, proton or neutron.
      LST(22)=1
      K(2,2)=2212
      IF(PARI(11).GT.1.E-06) THEN
        IF(RLU(0).LT.(PARI(11)*(PQH(17,IH)-PNT(1,IH)-PNT(2,IH))+
     &  PNT(2,IH))/PQH(17,IH)) THEN
          LST(22)=2
          K(2,2)=2112
        ENDIF
      ENDIF

      RETURN
 1200 FORMAT(' Warning: LEPTOX is looping, cannot find allowed ',
     &'phase space point due to cuts,',/,
     &10X,'check, in particular, CUT(11) to CUT(14)')
 1300 FORMAT(' Warning: maximum violated by a factor ',F7.3,
     &' in event ',I7,/,' maximum increased by this factor to ',E12.3,
     &/,' Point of violation: x, y, Q**2, W**2 = ',4G10.3)
      END
