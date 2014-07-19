 
C*********************************************************************
 
C...PYWIDT
C...Calculates full and partial widths of resonances.
 
      SUBROUTINE PYWIDT(KFLR,SH,WDTP,WDTE)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      INTEGER PYK,PYCHGE,PYCOMP
C...Parameter statement to help give large particle numbers.
      PARAMETER (KSUSY1=1000000,KSUSY2=2000000,KTECHN=3000000,
     &KEXCIT=4000000,KDIMEN=5000000)
C...Commonblocks.
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT4/MWID(500),WIDS(500,5)
      COMMON/PYMSSM/IMSS(0:99),RMSS(0:99)
      COMMON/PYSSMT/ZMIX(4,4),UMIX(2,2),VMIX(2,2),SMZ(4),SMW(2),
     &SFMIX(16,4),ZMIXI(4,4),UMIXI(2,2),VMIXI(2,2)
      SAVE /PYDAT1/,/PYDAT2/,/PYDAT3/,/PYSUBS/,/PYPARS/,/PYINT1/,
     &/PYINT4/,/PYMSSM/,/PYSSMT/
C...Local arrays and saved variables.
      COMPLEX*16 ZMIXC(4,4),AL,BL,AR,BR,FL,FR
      DIMENSION WDTP(0:400),WDTE(0:400,0:5),MOFSV(3,2),WIDWSV(3,2),
     &WID2SV(3,2),WDTPP(0:400),WDTEP(0:400,0:5)
      SAVE MOFSV,WIDWSV,WID2SV
      DATA MOFSV/6*0/,WIDWSV/6*0D0/,WID2SV/6*0D0/
 
C...Compressed code and sign; mass.
      KFLA=IABS(KFLR)
      KFLS=ISIGN(1,KFLR)
      KC=PYCOMP(KFLA)
      SHR=SQRT(SH)
      PMR=PMAS(KC,1)
 
C...Reset width information.
      DO 110 I=0,MDCY(KC,3)
        WDTP(I)=0D0
        DO 100 J=0,5
          WDTE(I,J)=0D0
  100   CONTINUE
  110 CONTINUE
 
C...Allow for fudge factor to rescale resonance width.
      FUDGE=1D0
      IF(MSTP(110).NE.0.AND.(MWID(KC).EQ.1.OR.MWID(KC).EQ.2.OR.
     &(MWID(KC).EQ.3.AND.MINT(63).EQ.1))) THEN
        IF(MSTP(110).EQ.KFLA) THEN
          FUDGE=PARP(110)
        ELSEIF(MSTP(110).EQ.-1) THEN
          IF(KFLA.NE.6.AND.KFLA.NE.23.AND.KFLA.NE.24) FUDGE=PARP(110)
        ELSEIF(MSTP(110).EQ.-2) THEN
          FUDGE=PARP(110)
        ENDIF
      ENDIF
 
C...Not to be treated as a resonance: return.
      IF((MWID(KC).LE.0.OR.MWID(KC).GE.4).AND.KFLA.NE.21.AND.
     &KFLA.NE.22) THEN
        WDTP(0)=1D0
        WDTE(0,0)=1D0
        MINT(61)=0
        MINT(62)=0
        MINT(63)=0
        RETURN
 
C...Treatment as a resonance based on tabulated branching ratios.
      ELSEIF(MWID(KC).EQ.2.OR.(MWID(KC).EQ.3.AND.MINT(63).EQ.0)) THEN
C...Loop over possible decay channels; skip irrelevant ones.
        DO 120 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 120
 
C...Read out decay products and nominal masses.
          KFD1=KFDP(IDC,1)
          KFC1=PYCOMP(KFD1)
          IF(KCHG(KFC1,3).EQ.1) KFD1=KFLS*KFD1
          PM1=PMAS(KFC1,1)
          KFD2=KFDP(IDC,2)
          KFC2=PYCOMP(KFD2)
          IF(KCHG(KFC2,3).EQ.1) KFD2=KFLS*KFD2
          PM2=PMAS(KFC2,1)
          KFD3=KFDP(IDC,3)
          PM3=0D0
          IF(KFD3.NE.0) THEN
            KFC3=PYCOMP(KFD3)
            IF(KCHG(KFC3,3).EQ.1) KFD3=KFLS*KFD3
            PM3=PMAS(KFC3,1)
          ENDIF
 
C...Naive partial width and alternative threshold factors.
          WDTP(I)=PMAS(KC,2)*BRAT(IDC)*(SHR/PMR)
          IF(MDME(IDC,2).GE.51.AND.MDME(IDC,2).LE.53.AND.
     &    PM1+PM2+PM3.GE.SHR) THEN
             WDTP(I)=0D0
          ELSEIF(MDME(IDC,2).EQ.52.AND.KFD3.EQ.0) THEN
            WDTP(I)=WDTP(I)*SQRT(MAX(0D0,(SH-PM1**2-PM2**2)**2-
     &      4D0*PM1**2*PM2**2))/SH
          ELSEIF(MDME(IDC,2).EQ.52) THEN
            PMA=MAX(PM1,PM2,PM3)
            PMC=MIN(PM1,PM2,PM3)
            PMB=PM1+PM2+PM3-PMA-PMC
            PMBC=PMB+PMC+0.5D0*(SHR-PMA-PMC-PMC)
            PMAN=PMA**2/SH
            PMBN=PMB**2/SH
            PMCN=PMC**2/SH
            PMBCN=PMBC**2/SH
            WDTP(I)=WDTP(I)*SQRT(MAX(0D0,
     &      ((1D0-PMAN-PMBCN)**2-4D0*PMAN*PMBCN)*
     &      ((PMBCN-PMBN-PMCN)**2-4D0*PMBN*PMCN)))*
     &      ((SHR-PMA)**2-(PMB+PMC)**2)*
     &      (1D0+0.25D0*(PMA+PMB+PMC)/SHR)/
     &      ((1D0-PMBCN)*PMBCN*SH)
          ELSEIF(MDME(IDC,2).EQ.53.AND.KFD3.EQ.0) THEN
            WDTP(I)=WDTP(I)*SQRT(
     &      MAX(0D0,(SH-PM1**2-PM2**2)**2-4D0*PM1**2*PM2**2)/
     &      MAX(1D-4,(PMR**2-PM1**2-PM2**2)**2-4D0*PM1**2*PM2**2))
          ELSEIF(MDME(IDC,2).EQ.53) THEN
            PMA=MAX(PM1,PM2,PM3)
            PMC=MIN(PM1,PM2,PM3)
            PMB=PM1+PM2+PM3-PMA-PMC
            PMBC=PMB+PMC+0.5D0*(SHR-PMA-PMB-PMC)
            PMAN=PMA**2/SH
            PMBN=PMB**2/SH
            PMCN=PMC**2/SH
            PMBCN=PMBC**2/SH
            FACACT=SQRT(MAX(0D0,
     &      ((1D0-PMAN-PMBCN)**2-4D0*PMAN*PMBCN)*
     &      ((PMBCN-PMBN-PMCN)**2-4D0*PMBN*PMCN)))*
     &      ((SHR-PMA)**2-(PMB+PMC)**2)*
     &      (1D0+0.25D0*(PMA+PMB+PMC)/SHR)/
     &      ((1D0-PMBCN)*PMBCN*SH)
            PMBC=PMB+PMC+0.5D0*(PMR-PMA-PMB-PMC)
            PMAN=PMA**2/PMR**2
            PMBN=PMB**2/PMR**2
            PMCN=PMC**2/PMR**2
            PMBCN=PMBC**2/PMR**2
            FACNOM=SQRT(MAX(0D0,
     &      ((1D0-PMAN-PMBCN)**2-4D0*PMAN*PMBCN)*
     &      ((PMBCN-PMBN-PMCN)**2-4D0*PMBN*PMCN)))*
     &      ((PMR-PMA)**2-(PMB+PMC)**2)*
     &      (1D0+0.25D0*(PMA+PMB+PMC)/PMR)/
     &      ((1D0-PMBCN)*PMBCN*PMR**2)
            WDTP(I)=WDTP(I)*FACACT/MAX(1D-6,FACNOM)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
 
C...Calculate secondary width (at most two identical/opposite).
          WID2=1D0
          IF(MDME(IDC,1).GT.0) THEN
            IF(KFD2.EQ.KFD1) THEN
              IF(KCHG(KFC1,3).EQ.0) THEN
                WID2=WIDS(KFC1,1)
              ELSEIF(KFD1.GT.0) THEN
                WID2=WIDS(KFC1,4)
              ELSE
                WID2=WIDS(KFC1,5)
              ENDIF
              IF(KFD3.GT.0) THEN
                WID2=WID2*WIDS(KFC3,2)
              ELSEIF(KFD3.LT.0) THEN
                WID2=WID2*WIDS(KFC3,3)
              ENDIF
            ELSEIF(KFD2.EQ.-KFD1) THEN
              WID2=WIDS(KFC1,1)
              IF(KFD3.GT.0) THEN
                WID2=WID2*WIDS(KFC3,2)
              ELSEIF(KFD3.LT.0) THEN
                WID2=WID2*WIDS(KFC3,3)
              ENDIF
            ELSEIF(KFD3.EQ.KFD1) THEN
              IF(KCHG(KFC1,3).EQ.0) THEN
                WID2=WIDS(KFC1,1)
              ELSEIF(KFD1.GT.0) THEN
                WID2=WIDS(KFC1,4)
              ELSE
                WID2=WIDS(KFC1,5)
              ENDIF
              IF(KFD2.GT.0) THEN
                WID2=WID2*WIDS(KFC2,2)
              ELSEIF(KFD2.LT.0) THEN
                WID2=WID2*WIDS(KFC2,3)
              ENDIF
            ELSEIF(KFD3.EQ.-KFD1) THEN
              WID2=WIDS(KFC1,1)
              IF(KFD2.GT.0) THEN
                WID2=WID2*WIDS(KFC2,2)
              ELSEIF(KFD2.LT.0) THEN
                WID2=WID2*WIDS(KFC2,3)
              ENDIF
            ELSEIF(KFD3.EQ.KFD2) THEN
              IF(KCHG(KFC2,3).EQ.0) THEN
                WID2=WIDS(KFC2,1)
              ELSEIF(KFD2.GT.0) THEN
                WID2=WIDS(KFC2,4)
              ELSE
                WID2=WIDS(KFC2,5)
              ENDIF
              IF(KFD1.GT.0) THEN
                WID2=WID2*WIDS(KFC1,2)
              ELSEIF(KFD1.LT.0) THEN
                WID2=WID2*WIDS(KFC1,3)
              ENDIF
            ELSEIF(KFD3.EQ.-KFD2) THEN
              WID2=WIDS(KFC2,1)
              IF(KFD1.GT.0) THEN
                WID2=WID2*WIDS(KFC1,2)
              ELSEIF(KFD1.LT.0) THEN
                WID2=WID2*WIDS(KFC1,3)
              ENDIF
            ELSE
              IF(KFD1.GT.0) THEN
                WID2=WIDS(KFC1,2)
              ELSE
                WID2=WIDS(KFC1,3)
              ENDIF
              IF(KFD2.GT.0) THEN
                WID2=WID2*WIDS(KFC2,2)
              ELSE
                WID2=WID2*WIDS(KFC2,3)
              ENDIF
              IF(KFD3.GT.0) THEN
                WID2=WID2*WIDS(KFC3,2)
              ELSEIF(KFD3.LT.0) THEN
                WID2=WID2*WIDS(KFC3,3)
              ENDIF
            ENDIF
 
C...Store effective widths according to case.
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  120   CONTINUE
C...Return.
        MINT(61)=0
        MINT(62)=0
        MINT(63)=0
        RETURN
      ENDIF
 
C...Here begins detailed dynamical calculation of resonance widths.
C...Shared treatment of Higgs states.
      KFHIGG=25
      IHIGG=1
      IF(KFLA.EQ.35.OR.KFLA.EQ.36) THEN
        KFHIGG=KFLA
        IHIGG=KFLA-33
      ENDIF
 
C...Common electroweak and strong constants.
      XW=PARU(102)
      XWV=XW
      IF(MSTP(8).GE.2) XW=1D0-(PMAS(24,1)/PMAS(23,1))**2
      XW1=1D0-XW
      AEM=PYALEM(SH)
      IF(MSTP(8).GE.1) AEM=SQRT(2D0)*PARU(105)*PMAS(24,1)**2*XW/PARU(1)
      AS=PYALPS(SH)
      RADC=1D0+AS/PARU(1)
 
      IF(KFLA.EQ.6) THEN
C...t quark.
        FAC=(AEM/(16D0*XW))*(SH/PMAS(24,1)**2)*SHR
        RADCT=1D0-2.5D0*AS/PARU(1)
        DO 140 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 140
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 140
          WID2=1D0
          IF(I.GE.4.AND.I.LE.7) THEN
C...t -> W + q; including approximate QCD correction factor.
            WDTP(I)=FAC*VCKM(3,I-3)*RADCT*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0-RM2)**2+(1D0+RM2)*RM1-2D0*RM1**2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,2)
              IF(I.EQ.7) WID2=WID2*WIDS(7,2)
            ELSE
              WID2=WIDS(24,3)
              IF(I.EQ.7) WID2=WID2*WIDS(7,3)
            ENDIF
          ELSEIF(I.EQ.9) THEN
C...t -> H + b.
            WDTP(I)=FAC*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0+RM2-RM1)*(RM2*PARU(141)**2+1D0/PARU(141)**2)+4D0*RM2)
            WID2=WIDS(37,2)
            IF(KFLR.LT.0) WID2=WIDS(37,3)
CMRENNA++
          ELSEIF(I.GE.10.AND.I.LE.13.AND.IMSS(1).NE.0) THEN
C...t -> ~t + ~chi_i0, i = 1, 2, 3 or 4.
            BETA=ATAN(RMSS(5))
            SINB=SIN(BETA)
            TANW=SQRT(PARU(102)/(1D0-PARU(102)))
            ET=KCHG(6,1)/3D0
            T3L=SIGN(0.5D0,ET)
            KFC1=PYCOMP(KFDP(IDC,1))
            KFC2=PYCOMP(KFDP(IDC,2))
            PMNCHI=PMAS(KFC1,1)
            PMSTOP=PMAS(KFC2,1)
            IF(SHR.GT.PMNCHI+PMSTOP) THEN
              IZ=I-9
              DO 130 IK=1,4
                ZMIXC(IZ,IK)=DCMPLX(ZMIX(IZ,IK),ZMIXI(IZ,IK))
  130         CONTINUE
              AL=SHR*DCONJG(ZMIXC(IZ,4))/(2.0D0*PMAS(24,1)*SINB)
              AR=-ET*ZMIXC(IZ,1)*TANW
              BL=T3L*(ZMIXC(IZ,2)-ZMIXC(IZ,1)*TANW)-AR
              BR=AL
              FL=SFMIX(6,1)*AL+SFMIX(6,2)*AR
              FR=SFMIX(6,1)*BL+SFMIX(6,2)*BR
              PCM=SQRT((SH-(PMNCHI+PMSTOP)**2)*
     &        (SH-(PMNCHI-PMSTOP)**2))/(2D0*SHR)
              WDTP(I)=(0.5D0*PYALEM(SH)/PARU(102))*PCM*
     &        ((ABS(FL)**2+ABS(FR)**2)*(SH+PMNCHI**2-PMSTOP**2)+
     &        SMZ(IZ)*4D0*SHR*DBLE(FL*DCONJG(FR)))/SH
              IF(KFLR.GT.0) THEN
                WID2=WIDS(KFC1,2)*WIDS(KFC2,2)
              ELSE
                WID2=WIDS(KFC1,2)*WIDS(KFC2,3)
              ENDIF
            ENDIF
          ELSEIF(I.EQ.14.AND.IMSS(1).NE.0) THEN
C...t -> ~g + ~t
            KFC1=PYCOMP(KFDP(IDC,1))
            KFC2=PYCOMP(KFDP(IDC,2))
            PMNCHI=PMAS(KFC1,1)
            PMSTOP=PMAS(KFC2,1)
            IF(SHR.GT.PMNCHI+PMSTOP) THEN
              RL=SFMIX(6,1)
              RR=-SFMIX(6,2)
              PCM=SQRT((SH-(PMNCHI+PMSTOP)**2)*
     &        (SH-(PMNCHI-PMSTOP)**2))/(2D0*SHR)
              WDTP(I)=4D0/3D0*0.5D0*PYALPS(SH)*PCM*((RL**2+RR**2)*
     &        (SH+PMNCHI**2-PMSTOP**2)+PMNCHI*4D0*SHR*RL*RR)/SH
              IF(KFLR.GT.0) THEN
                WID2=WIDS(KFC1,2)*WIDS(KFC2,2)
              ELSE
                WID2=WIDS(KFC1,2)*WIDS(KFC2,3)
              ENDIF
            ENDIF
          ELSEIF(I.EQ.15.AND.IMSS(1).NE.0) THEN
C...t -> ~gravitino + ~t
            XMP2=RMSS(29)**2
            KFC1=PYCOMP(KFDP(IDC,1))
            XMGR2=PMAS(KFC1,1)**2
            WDTP(I)=SH**2*SHR/(96D0*PARU(1)*XMP2*XMGR2)*(1D0-RM2)**4
            KFC2=PYCOMP(KFDP(IDC,2))
            WID2=WIDS(KFC2,2)
            IF(KFLR.LT.0) WID2=WIDS(KFC2,3)
CMRENNA--
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  140   CONTINUE
 
      ELSEIF(KFLA.EQ.7) THEN
C...b' quark.
        FAC=(AEM/(16D0*XW))*(SH/PMAS(24,1)**2)*SHR
        DO 150 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 150
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 150
          WID2=1D0
          IF(I.GE.4.AND.I.LE.7) THEN
C...b' -> W + q.
            WDTP(I)=FAC*VCKM(I-3,4)*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0-RM2)**2+(1D0+RM2)*RM1-2D0*RM1**2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,3)
              IF(I.EQ.6) WID2=WID2*WIDS(6,2)
              IF(I.EQ.7) WID2=WID2*WIDS(8,2)
            ELSE
              WID2=WIDS(24,2)
              IF(I.EQ.6) WID2=WID2*WIDS(6,3)
              IF(I.EQ.7) WID2=WID2*WIDS(8,3)
            ENDIF
            WID2=WIDS(24,3)
            IF(KFLR.LT.0) WID2=WIDS(24,2)
          ELSEIF(I.EQ.9.OR.I.EQ.10) THEN
C...b' -> H + q.
            WDTP(I)=FAC*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0+RM2-RM1)*(PARU(141)**2+RM2/PARU(141)**2)+4D0*RM2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(37,3)
              IF(I.EQ.10) WID2=WID2*WIDS(6,2)
            ELSE
              WID2=WIDS(37,2)
              IF(I.EQ.10) WID2=WID2*WIDS(6,3)
            ENDIF
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  150   CONTINUE
 
      ELSEIF(KFLA.EQ.8) THEN
C...t' quark.
        FAC=(AEM/(16D0*XW))*(SH/PMAS(24,1)**2)*SHR
        DO 160 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 160
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 160
          WID2=1D0
          IF(I.GE.4.AND.I.LE.7) THEN
C...t' -> W + q.
            WDTP(I)=FAC*VCKM(4,I-3)*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0-RM2)**2+(1D0+RM2)*RM1-2D0*RM1**2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,2)
              IF(I.EQ.7) WID2=WID2*WIDS(7,2)
            ELSE
              WID2=WIDS(24,3)
              IF(I.EQ.7) WID2=WID2*WIDS(7,3)
            ENDIF
          ELSEIF(I.EQ.9.OR.I.EQ.10) THEN
C...t' -> H + q.
            WDTP(I)=FAC*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0+RM2-RM1)*(RM2*PARU(141)**2+1D0/PARU(141)**2)+4D0*RM2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(37,2)
              IF(I.EQ.10) WID2=WID2*WIDS(7,2)
            ELSE
              WID2=WIDS(37,3)
              IF(I.EQ.10) WID2=WID2*WIDS(7,3)
            ENDIF
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  160   CONTINUE
 
      ELSEIF(KFLA.EQ.17) THEN
C...tau' lepton.
        FAC=(AEM/(16D0*XW))*(SH/PMAS(24,1)**2)*SHR
        DO 170 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 170
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 170
          WID2=1D0
          IF(I.EQ.3) THEN
C...tau' -> W + nu'_tau.
            WDTP(I)=FAC*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0-RM2)**2+(1D0+RM2)*RM1-2D0*RM1**2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,3)
              WID2=WID2*WIDS(18,2)
            ELSE
              WID2=WIDS(24,2)
              WID2=WID2*WIDS(18,3)
            ENDIF
          ELSEIF(I.EQ.5) THEN
C...tau' -> H + nu'_tau.
            WDTP(I)=FAC*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0+RM2-RM1)*(PARU(141)**2+RM2/PARU(141)**2)+4D0*RM2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(37,3)
              WID2=WID2*WIDS(18,2)
            ELSE
              WID2=WIDS(37,2)
              WID2=WID2*WIDS(18,3)
            ENDIF
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  170   CONTINUE
 
      ELSEIF(KFLA.EQ.18) THEN
C...nu'_tau neutrino.
        FAC=(AEM/(16D0*XW))*(SH/PMAS(24,1)**2)*SHR
        DO 180 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 180
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 180
          WID2=1D0
          IF(I.EQ.2) THEN
C...nu'_tau -> W + tau'.
            WDTP(I)=FAC*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0-RM2)**2+(1D0+RM2)*RM1-2D0*RM1**2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,2)
              WID2=WID2*WIDS(17,2)
            ELSE
              WID2=WIDS(24,3)
              WID2=WID2*WIDS(17,3)
            ENDIF
          ELSEIF(I.EQ.3) THEN
C...nu'_tau -> H + tau'.
            WDTP(I)=FAC*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0+RM2-RM1)*(RM2*PARU(141)**2+1D0/PARU(141)**2)+4D0*RM2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(37,2)
              WID2=WID2*WIDS(17,2)
            ELSE
              WID2=WIDS(37,3)
              WID2=WID2*WIDS(17,3)
            ENDIF
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  180   CONTINUE
 
      ELSEIF(KFLA.EQ.21) THEN
C...QCD:
C***Note that widths are not given in dimensional quantities here.
        DO 190 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 190
          RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 190
          WID2=1D0
          IF(I.LE.8) THEN
C...QCD -> q + qbar
            WDTP(I)=(1D0+2D0*RM1)*SQRT(MAX(0D0,1D0-4D0*RM1))
            IF(I.EQ.6) WID2=WIDS(6,1)
            IF((I.EQ.7.OR.I.EQ.8)) WID2=WIDS(I,1)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  190   CONTINUE
 
      ELSEIF(KFLA.EQ.22) THEN
C...QED photon.
C***Note that widths are not given in dimensional quantities here.
        DO 200 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 200
          RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 200
          WID2=1D0
          IF(I.LE.8) THEN
C...QED -> q + qbar.
            EF=KCHG(I,1)/3D0
            FCOF=3D0*RADC
            IF(I.GE.6.AND.MSTP(35).GE.1) FCOF=FCOF*PYHFTH(SH,SH*RM1,1D0)
            WDTP(I)=FCOF*EF**2*(1D0+2D0*RM1)*SQRT(MAX(0D0,1D0-4D0*RM1))
            IF(I.EQ.6) WID2=WIDS(6,1)
            IF((I.EQ.7.OR.I.EQ.8)) WID2=WIDS(I,1)
          ELSEIF(I.LE.12) THEN
C...QED -> l+ + l-.
            EF=KCHG(9+2*(I-8),1)/3D0
            WDTP(I)=EF**2*(1D0+2D0*RM1)*SQRT(MAX(0D0,1D0-4D0*RM1))
            IF(I.EQ.12) WID2=WIDS(17,1)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  200   CONTINUE
 
      ELSEIF(KFLA.EQ.23) THEN
C...Z0:
        ICASE=1
        XWC=1D0/(16D0*XW*XW1)
        FAC=(AEM*XWC/3D0)*SHR
  210   CONTINUE
        IF(MINT(61).GE.1.AND.ICASE.EQ.2) THEN
          VINT(111)=0D0
          VINT(112)=0D0
          VINT(114)=0D0
        ENDIF
        IF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
          KFI=IABS(MINT(15))
          IF(KFI.GT.20) KFI=IABS(MINT(16))
          EI=KCHG(KFI,1)/3D0
          AI=SIGN(1D0,EI)
          VI=AI-4D0*EI*XWV
          SQMZ=PMAS(23,1)**2
          HZ=SHR*WDTP(0)
          IF(MSTP(43).EQ.1.OR.MSTP(43).EQ.3) VINT(111)=1D0
          IF(MSTP(43).EQ.3) VINT(112)=
     &    2D0*XWC*SH*(SH-SQMZ)/((SH-SQMZ)**2+HZ**2)
          IF(MSTP(43).EQ.2.OR.MSTP(43).EQ.3) VINT(114)=
     &    XWC**2*SH**2/((SH-SQMZ)**2+HZ**2)
        ENDIF
        DO 220 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 220
          RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 220
          WID2=1D0
          IF(I.LE.8) THEN
C...Z0 -> q + qbar
            EF=KCHG(I,1)/3D0
            AF=SIGN(1D0,EF+0.1D0)
            VF=AF-4D0*EF*XWV
            FCOF=3D0*RADC
            IF(I.GE.6.AND.MSTP(35).GE.1) FCOF=FCOF*PYHFTH(SH,SH*RM1,1D0)
            IF(I.EQ.6) WID2=WIDS(6,1)
            IF((I.EQ.7.OR.I.EQ.8)) WID2=WIDS(I,1)
          ELSEIF(I.LE.16) THEN
C...Z0 -> l+ + l-, nu + nubar
            EF=KCHG(I+2,1)/3D0
            AF=SIGN(1D0,EF+0.1D0)
            VF=AF-4D0*EF*XWV
            FCOF=1D0
            IF((I.EQ.15.OR.I.EQ.16)) WID2=WIDS(2+I,1)
          ENDIF
          BE34=SQRT(MAX(0D0,1D0-4D0*RM1))
          IF(ICASE.EQ.1) THEN
            WDTP(I)=FAC*FCOF*(VF**2*(1D0+2D0*RM1)+AF**2*(1D0-4D0*RM1))*
     &      BE34
          ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
            WDTP(I)=FAC*FCOF*((EI**2*VINT(111)*EF**2+EI*VI*VINT(112)*
     &      EF*VF+(VI**2+AI**2)*VINT(114)*VF**2)*(1D0+2D0*RM1)+
     &      (VI**2+AI**2)*VINT(114)*AF**2*(1D0-4D0*RM1))*BE34
          ELSEIF(MINT(61).EQ.2.AND.ICASE.EQ.2) THEN
            FGGF=FCOF*EF**2*(1D0+2D0*RM1)*BE34
            FGZF=FCOF*EF*VF*(1D0+2D0*RM1)*BE34
            FZZF=FCOF*(VF**2*(1D0+2D0*RM1)+AF**2*(1D0-4D0*RM1))*BE34
          ENDIF
          IF(ICASE.EQ.1) WDTP(I)=FUDGE*WDTP(I)
          IF(ICASE.EQ.1) WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            IF((ICASE.EQ.1.AND.MINT(61).NE.1).OR.
     &      (ICASE.EQ.2.AND.MINT(61).EQ.1)) THEN
              WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
              WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+
     &        WDTE(I,MDME(IDC,1))
              WDTE(I,0)=WDTE(I,MDME(IDC,1))
              WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
            ENDIF
            IF(MINT(61).EQ.2.AND.ICASE.EQ.2) THEN
              IF(MSTP(43).EQ.1.OR.MSTP(43).EQ.3) VINT(111)=
     &        VINT(111)+FGGF*WID2
              IF(MSTP(43).EQ.3) VINT(112)=VINT(112)+FGZF*WID2
              IF(MSTP(43).EQ.2.OR.MSTP(43).EQ.3) VINT(114)=
     &        VINT(114)+FZZF*WID2
            ENDIF
          ENDIF
  220   CONTINUE
        IF(MINT(61).GE.1) ICASE=3-ICASE
        IF(ICASE.EQ.2) GOTO 210
 
      ELSEIF(KFLA.EQ.24) THEN
C...W+/-:
        FAC=(AEM/(24D0*XW))*SHR
        DO 230 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 230
          RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 230
          WID2=1D0
          IF(I.LE.16) THEN
C...W+/- -> q + qbar'
            FCOF=3D0*RADC*VCKM((I-1)/4+1,MOD(I-1,4)+1)
            IF(KFLR.GT.0) THEN
              IF(MOD(I,4).EQ.3) WID2=WIDS(6,2)
              IF(MOD(I,4).EQ.0) WID2=WIDS(8,2)
              IF(I.GE.13) WID2=WID2*WIDS(7,3)
            ELSE
              IF(MOD(I,4).EQ.3) WID2=WIDS(6,3)
              IF(MOD(I,4).EQ.0) WID2=WIDS(8,3)
              IF(I.GE.13) WID2=WID2*WIDS(7,2)
            ENDIF
          ELSEIF(I.LE.20) THEN
C...W+/- -> l+/- + nu
            FCOF=1D0
            IF(KFLR.GT.0) THEN
              IF(I.EQ.20) WID2=WIDS(17,3)*WIDS(18,2)
            ELSE
              IF(I.EQ.20) WID2=WIDS(17,2)*WIDS(18,3)
            ENDIF
          ENDIF
          WDTP(I)=FAC*FCOF*(2D0-RM1-RM2-(RM1-RM2)**2)*
     &    SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  230   CONTINUE
 
      ELSEIF(KFLA.EQ.25.OR.KFLA.EQ.35.OR.KFLA.EQ.36) THEN
C...h0 (or H0, or A0):
        SHFS=SH
        FAC=(AEM/(8D0*XW))*(SHFS/PMAS(24,1)**2)*SHR
        DO 270 I=1,MDCY(KFHIGG,3)
          IDC=I+MDCY(KFHIGG,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 270
          KFC1=PYCOMP(KFDP(IDC,1))
          KFC2=PYCOMP(KFDP(IDC,2))
          RM1=PMAS(KFC1,1)**2/SH
          RM2=PMAS(KFC2,1)**2/SH
          IF(I.NE.16.AND.I.NE.17.AND.SQRT(RM1)+SQRT(RM2).GT.1D0)
     &    GOTO 270
          WID2=1D0
 
          IF(I.LE.8) THEN
C...h0 -> q + qbar
            WDTP(I)=FAC*3D0*(PYMRUN(KFDP(IDC,1),SH)**2/SHFS)*
     &      SQRT(MAX(0D0,1D0-4D0*RM1))*RADC
C...A0 behaves like beta, ho and H0 like beta**3.
            IF(IHIGG.NE.3) WDTP(I)=WDTP(I)*(1D0-4D0*RM1)
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
              IF(MOD(I,2).EQ.1) WDTP(I)=WDTP(I)*PARU(151+10*IHIGG)**2
              IF(MOD(I,2).EQ.0) WDTP(I)=WDTP(I)*PARU(152+10*IHIGG)**2
              IF(IMSS(1).NE.0.AND.KFC1.EQ.5) THEN
                WDTP(I)=WDTP(I)/(1D0+RMSS(41))**2
                IF(IHIGG.NE.3) THEN
                  WDTP(I)=WDTP(I)*(1D0+RMSS(41)*PARU(152+10*IHIGG)/
     &            PARU(151+10*IHIGG))**2
                ENDIF
              ENDIF
            ENDIF
            IF(I.EQ.6) WID2=WIDS(6,1)
            IF((I.EQ.7.OR.I.EQ.8)) WID2=WIDS(I,1)
          ELSEIF(I.LE.12) THEN
C...h0 -> l+ + l-
            WDTP(I)=FAC*RM1*SQRT(MAX(0D0,1D0-4D0*RM1))*(SH/SHFS)
C...A0 behaves like beta, ho and H0 like beta**3.
            IF(IHIGG.NE.3) WDTP(I)=WDTP(I)*(1D0-4D0*RM1)
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) WDTP(I)=WDTP(I)*
     &      PARU(153+10*IHIGG)**2
            IF(I.EQ.12) WID2=WIDS(17,1)
 
          ELSEIF(I.EQ.13) THEN
C...h0 -> g + g; quark loop contribution only
            ETARE=0D0
            ETAIM=0D0
            DO 240 J=1,2*MSTP(1)
              EPS=(2D0*PMAS(J,1))**2/SH
C...Loop integral; function of eps=4m^2/shat; different for A0.
              IF(EPS.LE.1D0) THEN
                IF(EPS.GT.1D-4) THEN
                  ROOT=SQRT(1D0-EPS)
                  RLN=LOG((1D0+ROOT)/(1D0-ROOT))
                ELSE
                  RLN=LOG(4D0/EPS-2D0)
                ENDIF
                PHIRE=-0.25D0*(RLN**2-PARU(1)**2)
                PHIIM=0.5D0*PARU(1)*RLN
              ELSE
                PHIRE=(ASIN(1D0/SQRT(EPS)))**2
                PHIIM=0D0
              ENDIF
              IF(IHIGG.LE.2) THEN
                ETAREJ=-0.5D0*EPS*(1D0+(1D0-EPS)*PHIRE)
                ETAIMJ=-0.5D0*EPS*(1D0-EPS)*PHIIM
              ELSE
                ETAREJ=-0.5D0*EPS*PHIRE
                ETAIMJ=-0.5D0*EPS*PHIIM
              ENDIF
C...Couplings (=1 for standard model Higgs).
              IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
                IF(MOD(J,2).EQ.1) THEN
                  ETAREJ=ETAREJ*PARU(151+10*IHIGG)
                  ETAIMJ=ETAIMJ*PARU(151+10*IHIGG)
                ELSE
                  ETAREJ=ETAREJ*PARU(152+10*IHIGG)
                  ETAIMJ=ETAIMJ*PARU(152+10*IHIGG)
                ENDIF
              ENDIF
              ETARE=ETARE+ETAREJ
              ETAIM=ETAIM+ETAIMJ
  240       CONTINUE
            ETA2=ETARE**2+ETAIM**2
            WDTP(I)=FAC*(AS/PARU(1))**2*ETA2
 
          ELSEIF(I.EQ.14) THEN
C...h0 -> gamma + gamma; quark, lepton, W+- and H+- loop contributions
            ETARE=0D0
            ETAIM=0D0
            JMAX=3*MSTP(1)+1
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) JMAX=JMAX+1
            DO 250 J=1,JMAX
              IF(J.LE.2*MSTP(1)) THEN
                EJ=KCHG(J,1)/3D0
                EPS=(2D0*PMAS(J,1))**2/SH
              ELSEIF(J.LE.3*MSTP(1)) THEN
                JL=2*(J-2*MSTP(1))-1
                EJ=KCHG(10+JL,1)/3D0
                EPS=(2D0*PMAS(10+JL,1))**2/SH
              ELSEIF(J.EQ.3*MSTP(1)+1) THEN
                EPS=(2D0*PMAS(24,1))**2/SH
              ELSE
                EPS=(2D0*PMAS(37,1))**2/SH
              ENDIF
C...Loop integral; function of eps=4m^2/shat.
              IF(EPS.LE.1D0) THEN
                IF(EPS.GT.1D-4) THEN
                  ROOT=SQRT(1D0-EPS)
                  RLN=LOG((1D0+ROOT)/(1D0-ROOT))
                ELSE
                  RLN=LOG(4D0/EPS-2D0)
                ENDIF
                PHIRE=-0.25D0*(RLN**2-PARU(1)**2)
                PHIIM=0.5D0*PARU(1)*RLN
              ELSE
                PHIRE=(ASIN(1D0/SQRT(EPS)))**2
                PHIIM=0D0
              ENDIF
              IF(J.LE.3*MSTP(1)) THEN
C...Fermion loops: loop integral different for A0; charges.
                IF(IHIGG.LE.2) THEN
                  PHIPRE=-0.5D0*EPS*(1D0+(1D0-EPS)*PHIRE)
                  PHIPIM=-0.5D0*EPS*(1D0-EPS)*PHIIM
                ELSE
                  PHIPRE=-0.5D0*EPS*PHIRE
                  PHIPIM=-0.5D0*EPS*PHIIM
                ENDIF
                IF(J.LE.2*MSTP(1).AND.MOD(J,2).EQ.1) THEN
                  EJC=3D0*EJ**2
                  EJH=PARU(151+10*IHIGG)
                ELSEIF(J.LE.2*MSTP(1)) THEN
                  EJC=3D0*EJ**2
                  EJH=PARU(152+10*IHIGG)
                ELSE
                  EJC=EJ**2
                  EJH=PARU(153+10*IHIGG)
                ENDIF
                IF(MSTP(4).EQ.0.AND.IHIGG.EQ.1) EJH=1D0
                ETAREJ=EJC*EJH*PHIPRE
                ETAIMJ=EJC*EJH*PHIPIM
              ELSEIF(J.EQ.3*MSTP(1)+1) THEN
C...W loops: loop integral and charges.
                ETAREJ=0.5D0+0.75D0*EPS*(1D0+(2D0-EPS)*PHIRE)
                ETAIMJ=0.75D0*EPS*(2D0-EPS)*PHIIM
                IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
                  ETAREJ=ETAREJ*PARU(155+10*IHIGG)
                  ETAIMJ=ETAIMJ*PARU(155+10*IHIGG)
                ENDIF
              ELSE
C...Charged H loops: loop integral and charges.
                FACHHH=(PMAS(24,1)/PMAS(37,1))**2*
     &          PARU(158+10*IHIGG+2*(IHIGG/3))
                ETAREJ=EPS*(1D0-EPS*PHIRE)*FACHHH
                ETAIMJ=-EPS**2*PHIIM*FACHHH
              ENDIF
              ETARE=ETARE+ETAREJ
              ETAIM=ETAIM+ETAIMJ
  250       CONTINUE
            ETA2=ETARE**2+ETAIM**2
            WDTP(I)=FAC*(AEM/PARU(1))**2*0.5D0*ETA2
 
          ELSEIF(I.EQ.15) THEN
C...h0 -> gamma + Z0; quark, lepton, W and H+- loop contributions
            ETARE=0D0
            ETAIM=0D0
            JMAX=3*MSTP(1)+1
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) JMAX=JMAX+1
            DO 260 J=1,JMAX
              IF(J.LE.2*MSTP(1)) THEN
                EJ=KCHG(J,1)/3D0
                AJ=SIGN(1D0,EJ+0.1D0)
                VJ=AJ-4D0*EJ*XWV
                EPS=(2D0*PMAS(J,1))**2/SH
                EPSP=(2D0*PMAS(J,1)/PMAS(23,1))**2
              ELSEIF(J.LE.3*MSTP(1)) THEN
                JL=2*(J-2*MSTP(1))-1
                EJ=KCHG(10+JL,1)/3D0
                AJ=SIGN(1D0,EJ+0.1D0)
                VJ=AJ-4D0*EJ*XWV
                EPS=(2D0*PMAS(10+JL,1))**2/SH
                EPSP=(2D0*PMAS(10+JL,1)/PMAS(23,1))**2
              ELSE
                EPS=(2D0*PMAS(24,1))**2/SH
                EPSP=(2D0*PMAS(24,1)/PMAS(23,1))**2
              ENDIF
C...Loop integrals; functions of eps=4m^2/shat and eps'=4m^2/m_Z^2.
              IF(EPS.LE.1D0) THEN
                ROOT=SQRT(1D0-EPS)
                IF(EPS.GT.1D-4) THEN
                  RLN=LOG((1D0+ROOT)/(1D0-ROOT))
                ELSE
                  RLN=LOG(4D0/EPS-2D0)
                ENDIF
                PHIRE=-0.25D0*(RLN**2-PARU(1)**2)
                PHIIM=0.5D0*PARU(1)*RLN
                PSIRE=0.5D0*ROOT*RLN
                PSIIM=-0.5D0*ROOT*PARU(1)
              ELSE
                PHIRE=(ASIN(1D0/SQRT(EPS)))**2
                PHIIM=0D0
                PSIRE=SQRT(EPS-1D0)*ASIN(1D0/SQRT(EPS))
                PSIIM=0D0
              ENDIF
              IF(EPSP.LE.1D0) THEN
                ROOT=SQRT(1D0-EPSP)
                IF(EPSP.GT.1D-4) THEN
                  RLN=LOG((1D0+ROOT)/(1D0-ROOT))
                ELSE
                  RLN=LOG(4D0/EPSP-2D0)
                ENDIF
                PHIREP=-0.25D0*(RLN**2-PARU(1)**2)
                PHIIMP=0.5D0*PARU(1)*RLN
                PSIREP=0.5D0*ROOT*RLN
                PSIIMP=-0.5D0*ROOT*PARU(1)
              ELSE
                PHIREP=(ASIN(1D0/SQRT(EPSP)))**2
                PHIIMP=0D0
                PSIREP=SQRT(EPSP-1D0)*ASIN(1D0/SQRT(EPSP))
                PSIIMP=0D0
              ENDIF
              FXYRE=EPS*EPSP/(8D0*(EPS-EPSP))*(1D0+EPS*EPSP/(EPS-EPSP)*
     &        (PHIRE-PHIREP)+2D0*EPS/(EPS-EPSP)*(PSIRE-PSIREP))
              FXYIM=EPS**2*EPSP/(8D0*(EPS-EPSP)**2)*
     &        (EPSP*(PHIIM-PHIIMP)+2D0*(PSIIM-PSIIMP))
              F1RE=-EPS*EPSP/(2D0*(EPS-EPSP))*(PHIRE-PHIREP)
              F1IM=-EPS*EPSP/(2D0*(EPS-EPSP))*(PHIIM-PHIIMP)
              IF(J.LE.3*MSTP(1)) THEN
C...Fermion loops: loop integral different for A0; charges.
                IF(IHIGG.EQ.3) FXYRE=0D0
                IF(IHIGG.EQ.3) FXYIM=0D0
                IF(J.LE.2*MSTP(1).AND.MOD(J,2).EQ.1) THEN
                  EJC=-3D0*EJ*VJ
                  EJH=PARU(151+10*IHIGG)
                ELSEIF(J.LE.2*MSTP(1)) THEN
                  EJC=-3D0*EJ*VJ
                  EJH=PARU(152+10*IHIGG)
                ELSE
                  EJC=-EJ*VJ
                  EJH=PARU(153+10*IHIGG)
                ENDIF
                IF(MSTP(4).EQ.0.AND.IHIGG.EQ.1) EJH=1D0
                ETAREJ=EJC*EJH*(FXYRE-0.25D0*F1RE)
                ETAIMJ=EJC*EJH*(FXYIM-0.25D0*F1IM)
              ELSEIF(J.EQ.3*MSTP(1)+1) THEN
C...W loops: loop integral and charges.
                HEPS=(1D0+2D0/EPS)*XW/XW1-(5D0+2D0/EPS)
                ETAREJ=-XW1*((3D0-XW/XW1)*F1RE+HEPS*FXYRE)
                ETAIMJ=-XW1*((3D0-XW/XW1)*F1IM+HEPS*FXYIM)
                IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
                  ETAREJ=ETAREJ*PARU(155+10*IHIGG)
                  ETAIMJ=ETAIMJ*PARU(155+10*IHIGG)
                ENDIF
              ELSE
C...Charged H loops: loop integral and charges.
                FACHHH=(PMAS(24,1)/PMAS(37,1))**2*(1D0-2D0*XW)*
     &          PARU(158+10*IHIGG+2*(IHIGG/3))
                ETAREJ=FACHHH*FXYRE
                ETAIMJ=FACHHH*FXYIM
              ENDIF
              ETARE=ETARE+ETAREJ
              ETAIM=ETAIM+ETAIMJ
  260       CONTINUE
            ETA2=(ETARE**2+ETAIM**2)/(XW*XW1)
            WDTP(I)=FAC*(AEM/PARU(1))**2*(1D0-PMAS(23,1)**2/SH)**3*ETA2
            WID2=WIDS(23,2)
 
          ELSEIF(I.LE.17) THEN
C...h0 -> Z0 + Z0, W+ + W-
            PM1=PMAS(IABS(KFDP(IDC,1)),1)
            PG1=PMAS(IABS(KFDP(IDC,1)),2)
            IF(MINT(62).GE.1) THEN
              IF(MSTP(42).EQ.0.OR.(4D0*(PM1+10D0*PG1)**2.LT.SH.AND.
     &        CKIN(46).LT.CKIN(45).AND.CKIN(48).LT.CKIN(47).AND.
     &        MAX(CKIN(45),CKIN(47)).LT.PM1-10D0*PG1)) THEN
                MOFSV(IHIGG,I-15)=0
                WIDW=(1D0-4D0*RM1+12D0*RM1**2)*SQRT(MAX(0D0,
     &          1D0-4D0*RM1))
                WID2=1D0
              ELSE
                MOFSV(IHIGG,I-15)=1
                RMAS=SQRT(MAX(0D0,SH))
                CALL PYOFSH(1,KFLA,KFDP(IDC,1),KFDP(IDC,2),RMAS,WIDW,
     &          WID2)
                WIDWSV(IHIGG,I-15)=WIDW
                WID2SV(IHIGG,I-15)=WID2
              ENDIF
            ELSE
              IF(MOFSV(IHIGG,I-15).EQ.0) THEN
                WIDW=(1D0-4D0*RM1+12D0*RM1**2)*SQRT(MAX(0D0,
     &          1D0-4D0*RM1))
                WID2=1D0
              ELSE
                WIDW=WIDWSV(IHIGG,I-15)
                WID2=WID2SV(IHIGG,I-15)
              ENDIF
            ENDIF
            WDTP(I)=FAC*WIDW/(2D0*(18-I))
            IF(MSTP(49).NE.0) WDTP(I)=WDTP(I)*PMAS(KFHIGG,1)**2/SHFS
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) WDTP(I)=WDTP(I)*
     &      PARU(138+I+10*IHIGG)**2
            WID2=WID2*WIDS(7+I,1)
 
          ELSEIF(I.EQ.18.AND.IHIGG.GE.2) THEN
C...H0 -> Z0 + h0, A0-> Z0 + h0
            WDTP(I)=FAC*0.5D0*SQRT(MAX(0D0,
     &      (1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
            IF(IHIGG.EQ.2) THEN
             WDTP(I)=WDTP(I)*PARU(179)**2
            ELSEIF(IHIGG.EQ.3) THEN
             WDTP(I)=WDTP(I)*PARU(186)**2
            ENDIF
            WID2=WIDS(23,2)*WIDS(25,2)
 
          ELSEIF(I.EQ.19.AND.IHIGG.GE.2) THEN
C...H0 -> h0 + h0, A0-> h0 + h0
            WDTP(I)=FAC*0.25D0*
     &      PMAS(23,1)**4/SH**2*SQRT(MAX(0D0,1D0-4D0*RM1))
            IF(IHIGG.EQ.2) THEN
             WDTP(I)=WDTP(I)*PARU(176)**2
            ELSEIF(IHIGG.EQ.3) THEN
             WDTP(I)=WDTP(I)*PARU(169)**2
            ENDIF
            WID2=WIDS(25,1)
          ELSEIF((I.EQ.20.OR.I.EQ.21).AND.IHIGG.GE.2) THEN
C...H0 -> W+/- + H-/+, A0 -> W+/- + H-/+
            WDTP(I)=FAC*0.5D0*SQRT(MAX(0D0,
     &      (1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
     &      *PARU(195+IHIGG)**2
            IF(I.EQ.20) THEN
              WID2=WIDS(24,2)*WIDS(37,3)
            ELSEIF(I.EQ.21) THEN
              WID2=WIDS(24,3)*WIDS(37,2)
            ENDIF
 
          ELSEIF(I.EQ.22.AND.IHIGG.EQ.2) THEN
C...H0 -> Z0 + A0.
            WDTP(I)=FAC*0.5D0*PARU(187)**2*SQRT(MAX(0D0,
     &      (1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*0.0D0
            WID2=WIDS(36,2)*WIDS(23,2)
 
          ELSEIF(I.EQ.23.AND.IHIGG.EQ.2) THEN
C...H0 -> h0 + A0.
            WDTP(I)=FAC*0.5D0*PARU(180)**2*
     &      PMAS(23,1)**4/SH**2*SQRT(MAX(0D0,1D0-4D0*RM1))
            WID2=WIDS(25,2)*WIDS(36,2)
 
          ELSEIF(I.EQ.24.AND.IHIGG.EQ.2) THEN
C...H0 -> A0 + A0
            WDTP(I)=FAC*0.25D0*PARU(177)**2*
     &      PMAS(23,1)**4/SH**2*SQRT(MAX(0D0,1D0-4D0*RM1))
            WID2=WIDS(36,1)
 
CMRENNA++
          ELSE
C...Add in SUSY decays (two-body) by rescaling by phase space factor.
            RM10=RM1*SH/PMR**2
            RM20=RM2*SH/PMR**2
            WFAC0=1D0+RM10**2+RM20**2-2D0*(RM10+RM20+RM10*RM20)
            WFAC=1D0+RM1**2+RM2**2-2D0*(RM1+RM2+RM1*RM2)
            IF(WFAC.LE.0D0 .OR. WFAC0.LE.0D0) THEN
              WFAC=0D0
            ELSE
              WFAC=WFAC/WFAC0
            ENDIF
            WDTP(I)=PMAS(KFLA,2)*BRAT(IDC)*(SHR/PMR)*SQRT(WFAC)
CMRENNA--
            IF(KFC2.EQ.KFC1) THEN
              WID2=WIDS(KFC1,1)
            ELSE
              KSGN1=2
              IF(KFDP(IDC,1).LT.0) KSGN1=3
              KSGN2=2
              IF(KFDP(IDC,2).LT.0) KSGN2=3
              WID2=WIDS(KFC1,KSGN1)*WIDS(KFC2,KSGN2)
            ENDIF
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  270   CONTINUE
 
      ELSEIF(KFLA.EQ.32) THEN
C...Z'0:
        ICASE=1
        XWC=1D0/(16D0*XW*XW1)
        FAC=(AEM*XWC/3D0)*SHR
        VINT(117)=0D0
  280   CONTINUE
        IF(MINT(61).GE.1.AND.ICASE.EQ.2) THEN
          VINT(111)=0D0
          VINT(112)=0D0
          VINT(113)=0D0
          VINT(114)=0D0
          VINT(115)=0D0
          VINT(116)=0D0
        ENDIF
        IF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
          KFAI=IABS(MINT(15))
          EI=KCHG(KFAI,1)/3D0
          AI=SIGN(1D0,EI+0.1D0)
          VI=AI-4D0*EI*XWV
          KFAIC=1
          IF(KFAI.LE.10.AND.MOD(KFAI,2).EQ.0) KFAIC=2
          IF(KFAI.GT.10.AND.MOD(KFAI,2).NE.0) KFAIC=3
          IF(KFAI.GT.10.AND.MOD(KFAI,2).EQ.0) KFAIC=4
          IF(KFAI.LE.2.OR.KFAI.EQ.11.OR.KFAI.EQ.12) THEN
            VPI=PARU(119+2*KFAIC)
            API=PARU(120+2*KFAIC)
          ELSEIF(KFAI.LE.4.OR.KFAI.EQ.13.OR.KFAI.EQ.14) THEN
            VPI=PARJ(178+2*KFAIC)
            API=PARJ(179+2*KFAIC)
          ELSE
            VPI=PARJ(186+2*KFAIC)
            API=PARJ(187+2*KFAIC)
          ENDIF
          SQMZ=PMAS(23,1)**2
          HZ=SHR*VINT(117)
          SQMZP=PMAS(32,1)**2
          HZP=SHR*WDTP(0)
          IF(MSTP(44).EQ.1.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.5.OR.
     &    MSTP(44).EQ.7) VINT(111)=1D0
          IF(MSTP(44).EQ.4.OR.MSTP(44).EQ.7) VINT(112)=
     &    2D0*XWC*SH*(SH-SQMZ)/((SH-SQMZ)**2+HZ**2)
          IF(MSTP(44).EQ.5.OR.MSTP(44).EQ.7) VINT(113)=
     &    2D0*XWC*SH*(SH-SQMZP)/((SH-SQMZP)**2+HZP**2)
          IF(MSTP(44).EQ.2.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.6.OR.
     &    MSTP(44).EQ.7) VINT(114)=XWC**2*SH**2/((SH-SQMZ)**2+HZ**2)
          IF(MSTP(44).EQ.6.OR.MSTP(44).EQ.7) VINT(115)=
     &    2D0*XWC**2*SH**2*((SH-SQMZ)*(SH-SQMZP)+HZ*HZP)/
     &    (((SH-SQMZ)**2+HZ**2)*((SH-SQMZP)**2+HZP**2))
          IF(MSTP(44).EQ.3.OR.MSTP(44).EQ.5.OR.MSTP(44).EQ.6.OR.
     &    MSTP(44).EQ.7) VINT(116)=XWC**2*SH**2/((SH-SQMZP)**2+HZP**2)
        ENDIF
        DO 290 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 290
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0.OR.MDME(IDC,1).LT.0) GOTO 290
          WID2=1D0
          IF(I.LE.16) THEN
            IF(I.LE.8) THEN
C...Z'0 -> q + qbar
              EF=KCHG(I,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
              IF(I.LE.2) THEN
                VPF=PARU(123-2*MOD(I,2))
                APF=PARU(124-2*MOD(I,2))
              ELSEIF(I.LE.4) THEN
                VPF=PARJ(182-2*MOD(I,2))
                APF=PARJ(183-2*MOD(I,2))
              ELSE
                VPF=PARJ(190-2*MOD(I,2))
                APF=PARJ(191-2*MOD(I,2))
              ENDIF
              FCOF=3D0*RADC
              IF(I.GE.6.AND.MSTP(35).GE.1) FCOF=FCOF*
     &        PYHFTH(SH,SH*RM1,1D0)
              IF(I.EQ.6) WID2=WIDS(6,1)
              IF((I.EQ.7.OR.I.EQ.8)) WID2=WIDS(I,1)
            ELSEIF(I.LE.16) THEN
C...Z'0 -> l+ + l-, nu + nubar
              EF=KCHG(I+2,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
              IF(I.LE.10) THEN
                VPF=PARU(127-2*MOD(I,2))
                APF=PARU(128-2*MOD(I,2))
              ELSEIF(I.LE.12) THEN
                VPF=PARJ(186-2*MOD(I,2))
                APF=PARJ(187-2*MOD(I,2))
              ELSE
                VPF=PARJ(194-2*MOD(I,2))
                APF=PARJ(195-2*MOD(I,2))
              ENDIF
              FCOF=1D0
              IF((I.EQ.15.OR.I.EQ.16)) WID2=WIDS(2+I,1)
            ENDIF
            BE34=SQRT(MAX(0D0,1D0-4D0*RM1))
            IF(ICASE.EQ.1) THEN
              WDTPZ=FCOF*(VF**2*(1D0+2D0*RM1)+AF**2*(1D0-4D0*RM1))*BE34
              WDTP(I)=FAC*FCOF*(VPF**2*(1D0+2D0*RM1)+
     &        APF**2*(1D0-4D0*RM1))*BE34
            ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
              WDTP(I)=FAC*FCOF*((EI**2*VINT(111)*EF**2+EI*VI*VINT(112)*
     &        EF*VF+EI*VPI*VINT(113)*EF*VPF+(VI**2+AI**2)*VINT(114)*
     &        VF**2+(VI*VPI+AI*API)*VINT(115)*VF*VPF+(VPI**2+API**2)*
     &        VINT(116)*VPF**2)*(1D0+2D0*RM1)+((VI**2+AI**2)*VINT(114)*
     &        AF**2+(VI*VPI+AI*API)*VINT(115)*AF*APF+(VPI**2+API**2)*
     &        VINT(116)*APF**2)*(1D0-4D0*RM1))*BE34
            ELSEIF(MINT(61).EQ.2) THEN
              FGGF=FCOF*EF**2*(1D0+2D0*RM1)*BE34
              FGZF=FCOF*EF*VF*(1D0+2D0*RM1)*BE34
              FGZPF=FCOF*EF*VPF*(1D0+2D0*RM1)*BE34
              FZZF=FCOF*(VF**2*(1D0+2D0*RM1)+AF**2*(1D0-4D0*RM1))*BE34
              FZZPF=FCOF*(VF*VPF*(1D0+2D0*RM1)+AF*APF*(1D0-4D0*RM1))*
     &        BE34
              FZPZPF=FCOF*(VPF**2*(1D0+2D0*RM1)+APF**2*(1D0-4D0*RM1))*
     &        BE34
            ENDIF
          ELSEIF(I.EQ.17) THEN
C...Z'0 -> W+ + W-
            WDTPZP=PARU(129)**2*XW1**2*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (1D0+10D0*RM1+10D0*RM2+RM1**2+RM2**2+10D0*RM1*RM2)
            IF(ICASE.EQ.1) THEN
              WDTPZ=0D0
              WDTP(I)=FAC*WDTPZP
            ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
              WDTP(I)=FAC*(VPI**2+API**2)*VINT(116)*WDTPZP
            ELSEIF(MINT(61).EQ.2) THEN
              FGGF=0D0
              FGZF=0D0
              FGZPF=0D0
              FZZF=0D0
              FZZPF=0D0
              FZPZPF=WDTPZP
            ENDIF
            WID2=WIDS(24,1)
          ELSEIF(I.EQ.18) THEN
C...Z'0 -> H+ + H-
            CZC=2D0*(1D0-2D0*XW)
            BE34C=(1D0-4D0*RM1)*SQRT(MAX(0D0,1D0-4D0*RM1))
            IF(ICASE.EQ.1) THEN
              WDTPZ=0.25D0*PARU(142)**2*CZC**2*BE34C
              WDTP(I)=FAC*0.25D0*PARU(143)**2*CZC**2*BE34C
            ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
              WDTP(I)=FAC*0.25D0*(EI**2*VINT(111)+PARU(142)*EI*VI*
     &        VINT(112)*CZC+PARU(143)*EI*VPI*VINT(113)*CZC+PARU(142)**2*
     &        (VI**2+AI**2)*VINT(114)*CZC**2+PARU(142)*PARU(143)*
     &        (VI*VPI+AI*API)*VINT(115)*CZC**2+PARU(143)**2*
     &        (VPI**2+API**2)*VINT(116)*CZC**2)*BE34C
            ELSEIF(MINT(61).EQ.2) THEN
              FGGF=0.25D0*BE34C
              FGZF=0.25D0*PARU(142)*CZC*BE34C
              FGZPF=0.25D0*PARU(143)*CZC*BE34C
              FZZF=0.25D0*PARU(142)**2*CZC**2*BE34C
              FZZPF=0.25D0*PARU(142)*PARU(143)*CZC**2*BE34C
              FZPZPF=0.25D0*PARU(143)**2*CZC**2*BE34C
            ENDIF
            WID2=WIDS(37,1)
          ELSEIF(I.EQ.19) THEN
C...Z'0 -> Z0 + gamma.
          ELSEIF(I.EQ.20) THEN
C...Z'0 -> Z0 + h0
            FLAM=SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
            WDTPZP=PARU(145)**2*4D0*ABS(1D0-2D0*XW)*
     &      (3D0*RM1+0.25D0*FLAM**2)*FLAM
            IF(ICASE.EQ.1) THEN
              WDTPZ=0D0
              WDTP(I)=FAC*WDTPZP
            ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
              WDTP(I)=FAC*(VPI**2+API**2)*VINT(116)*WDTPZP
            ELSEIF(MINT(61).EQ.2) THEN
              FGGF=0D0
              FGZF=0D0
              FGZPF=0D0
              FZZF=0D0
              FZZPF=0D0
              FZPZPF=WDTPZP
            ENDIF
            WID2=WIDS(23,2)*WIDS(25,2)
          ELSEIF(I.EQ.21.OR.I.EQ.22) THEN
C...Z' -> h0 + A0 or H0 + A0.
            BE34C=SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
            IF(I.EQ.21) THEN
              CZAH=PARU(186)
              CZPAH=PARU(188)
            ELSE
              CZAH=PARU(187)
              CZPAH=PARU(189)
            ENDIF
            IF(ICASE.EQ.1) THEN
              WDTPZ=CZAH**2*BE34C
              WDTP(I)=FAC*CZPAH**2*BE34C
            ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
              WDTP(I)=FAC*(CZAH**2*(VI**2+AI**2)*VINT(114)+CZAH*CZPAH*
     &        (VI*VPI+AI*API)*VINT(115)+CZPAH**2*(VPI**2+API**2)*
     &        VINT(116))*BE34C
            ELSEIF(MINT(61).EQ.2) THEN
              FGGF=0D0
              FGZF=0D0
              FGZPF=0D0
              FZZF=CZAH**2*BE34C
              FZZPF=CZAH*CZPAH*BE34C
              FZPZPF=CZPAH**2*BE34C
            ENDIF
            IF(I.EQ.21) WID2=WIDS(25,2)*WIDS(36,2)
            IF(I.EQ.22) WID2=WIDS(35,2)*WIDS(36,2)
          ENDIF
          IF(ICASE.EQ.1) THEN
            VINT(117)=VINT(117)+FAC*WDTPZ
            WDTP(I)=FUDGE*WDTP(I)
            WDTP(0)=WDTP(0)+WDTP(I)
          ENDIF
          IF(MDME(IDC,1).GT.0) THEN
            IF((ICASE.EQ.1.AND.MINT(61).NE.1).OR.
     &      (ICASE.EQ.2.AND.MINT(61).EQ.1)) THEN
              WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
              WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+
     &        WDTE(I,MDME(IDC,1))
              WDTE(I,0)=WDTE(I,MDME(IDC,1))
              WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
            ENDIF
            IF(MINT(61).EQ.2.AND.ICASE.EQ.2) THEN
              IF(MSTP(44).EQ.1.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.5.OR.
     &        MSTP(44).EQ.7) VINT(111)=VINT(111)+FGGF*WID2
              IF(MSTP(44).EQ.4.OR.MSTP(44).EQ.7) VINT(112)=VINT(112)+
     &        FGZF*WID2
              IF(MSTP(44).EQ.5.OR.MSTP(44).EQ.7) VINT(113)=VINT(113)+
     &        FGZPF*WID2
              IF(MSTP(44).EQ.2.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.6.OR.
     &        MSTP(44).EQ.7) VINT(114)=VINT(114)+FZZF*WID2
              IF(MSTP(44).EQ.6.OR.MSTP(44).EQ.7) VINT(115)=VINT(115)+
     &        FZZPF*WID2
              IF(MSTP(44).EQ.3.OR.MSTP(44).EQ.5.OR.MSTP(44).EQ.6.OR.
     &        MSTP(44).EQ.7) VINT(116)=VINT(116)+FZPZPF*WID2
            ENDIF
          ENDIF
  290   CONTINUE
        IF(MINT(61).GE.1) ICASE=3-ICASE
        IF(ICASE.EQ.2) GOTO 280
 
      ELSEIF(KFLA.EQ.34) THEN
C...W'+/-:
        FAC=(AEM/(24D0*XW))*SHR
        DO 300 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 300
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 300
          WID2=1D0
          IF(I.LE.20) THEN
            IF(I.LE.16) THEN
C...W'+/- -> q + qbar'
              FCOF=3D0*RADC*(PARU(131)**2+PARU(132)**2)*
     &        VCKM((I-1)/4+1,MOD(I-1,4)+1)
              IF(KFLR.GT.0) THEN
                IF(MOD(I,4).EQ.3) WID2=WIDS(6,2)
                IF(MOD(I,4).EQ.0) WID2=WIDS(8,2)
                IF(I.GE.13) WID2=WID2*WIDS(7,3)
              ELSE
                IF(MOD(I,4).EQ.3) WID2=WIDS(6,3)
                IF(MOD(I,4).EQ.0) WID2=WIDS(8,3)
                IF(I.GE.13) WID2=WID2*WIDS(7,2)
              ENDIF
            ELSEIF(I.LE.20) THEN
C...W'+/- -> l+/- + nu
              FCOF=PARU(133)**2+PARU(134)**2
              IF(KFLR.GT.0) THEN
                IF(I.EQ.20) WID2=WIDS(17,3)*WIDS(18,2)
              ELSE
                IF(I.EQ.20) WID2=WIDS(17,2)*WIDS(18,3)
              ENDIF
            ENDIF
            WDTP(I)=FAC*FCOF*0.5D0*(2D0-RM1-RM2-(RM1-RM2)**2)*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
          ELSEIF(I.EQ.21) THEN
C...W'+/- -> W+/- + Z0
            WDTP(I)=FAC*PARU(135)**2*0.5D0*XW1*(RM1/RM2)*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (1D0+10D0*RM1+10D0*RM2+RM1**2+RM2**2+10D0*RM1*RM2)
            IF(KFLR.GT.0) WID2=WIDS(24,2)*WIDS(23,2)
            IF(KFLR.LT.0) WID2=WIDS(24,3)*WIDS(23,2)
          ELSEIF(I.EQ.23) THEN
C...W'+/- -> W+/- + h0
            FLAM=SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
            WDTP(I)=FAC*PARU(146)**2*2D0*(3D0*RM1+0.25D0*FLAM**2)*FLAM
            IF(KFLR.GT.0) WID2=WIDS(24,2)*WIDS(25,2)
            IF(KFLR.LT.0) WID2=WIDS(24,3)*WIDS(25,2)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  300   CONTINUE
 
      ELSEIF(KFLA.EQ.37) THEN
C...H+/-:
C        IF(MSTP(49).EQ.0) THEN
        SHFS=SH
C        ELSE
C          SHFS=PMAS(37,1)**2
C        ENDIF
        FAC=(AEM/(8D0*XW))*(SHFS/PMAS(24,1)**2)*SHR
        DO 310 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 310
          KFC1=PYCOMP(KFDP(IDC,1))
          KFC2=PYCOMP(KFDP(IDC,2))
          RM1=PMAS(KFC1,1)**2/SH
          RM2=PMAS(KFC2,1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 310
          WID2=1D0
          IF(I.LE.4) THEN
C...H+/- -> q + qbar'
            RM1R=PYMRUN(KFDP(IDC,1),SH)**2/SH
            RM2R=PYMRUN(KFDP(IDC,2),SH)**2/SH
            WDTP(I)=FAC*3D0*RADC*MAX(0D0,(RM1R*PARU(141)**2+
     &      RM2R/PARU(141)**2)*(1D0-RM1R-RM2R)-4D0*RM1R*RM2R)*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*(SH/SHFS)
            IF(KFLR.GT.0) THEN
              IF(I.EQ.3) WID2=WIDS(6,2)
              IF(I.EQ.4) WID2=WIDS(7,3)*WIDS(8,2)
            ELSE
              IF(I.EQ.3) WID2=WIDS(6,3)
              IF(I.EQ.4) WID2=WIDS(7,2)*WIDS(8,3)
            ENDIF
          ELSEIF(I.LE.8) THEN
C...H+/- -> l+/- + nu
            WDTP(I)=FAC*((RM1*PARU(141)**2+RM2/PARU(141)**2)*
     &      (1D0-RM1-RM2)-4D0*RM1*RM2)*SQRT(MAX(0D0,
     &      (1D0-RM1-RM2)**2-4D0*RM1*RM2))*(SH/SHFS)
            IF(KFLR.GT.0) THEN
              IF(I.EQ.8) WID2=WIDS(17,3)*WIDS(18,2)
            ELSE
              IF(I.EQ.8) WID2=WIDS(17,2)*WIDS(18,3)
            ENDIF
          ELSEIF(I.EQ.9) THEN
C...H+/- -> W+/- + h0.
            WDTP(I)=FAC*PARU(195)**2*0.5D0*SQRT(MAX(0D0,
     &      (1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
            IF(KFLR.GT.0) WID2=WIDS(24,2)*WIDS(25,2)
            IF(KFLR.LT.0) WID2=WIDS(24,3)*WIDS(25,2)
 
CMRENNA++
          ELSE
C...Add in SUSY decays (two-body) by rescaling by phase space factor.
            RM10=RM1*SH/PMR**2
            RM20=RM2*SH/PMR**2
            WFAC0=1D0+RM10**2+RM20**2-2D0*(RM10+RM20+RM10*RM20)
            WFAC=1D0+RM1**2+RM2**2-2D0*(RM1+RM2+RM1*RM2)
            IF(WFAC.LE.0D0 .OR. WFAC0.LE.0D0) THEN
              WFAC=0D0
            ELSE
              WFAC=WFAC/WFAC0
            ENDIF
            WDTP(I)=PMAS(KC,2)*BRAT(IDC)*(SHR/PMR)*SQRT(WFAC)
CMRENNA--
            KSGN1=2
            IF(KFLS*KFDP(IDC,1).LT.0.AND.KCHG(KFC1,3).EQ.1) KSGN1=3
            KSGN2=2
            IF(KFLS*KFDP(IDC,2).LT.0.AND.KCHG(KFC2,3).EQ.1) KSGN2=3
            WID2=WIDS(KFC1,KSGN1)*WIDS(KFC2,KSGN2)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  310   CONTINUE
 
      ELSEIF(KFLA.EQ.41) THEN
C...R:
        FAC=(AEM/(12D0*XW))*SHR
        DO 320 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 320
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 320
          WID2=1D0
          IF(I.LE.6) THEN
C...R -> q + qbar'
            FCOF=3D0*RADC
          ELSEIF(I.LE.9) THEN
C...R -> l+ + l'-
            FCOF=1D0
          ENDIF
          WDTP(I)=FAC*FCOF*(2D0-RM1-RM2-(RM1-RM2)**2)*
     &    SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
          IF(KFLR.GT.0) THEN
            IF(I.EQ.4) WID2=WIDS(6,3)
            IF(I.EQ.5) WID2=WIDS(7,3)
            IF(I.EQ.6) WID2=WIDS(6,2)*WIDS(8,3)
            IF(I.EQ.9) WID2=WIDS(17,3)
          ELSE
            IF(I.EQ.4) WID2=WIDS(6,2)
            IF(I.EQ.5) WID2=WIDS(7,2)
            IF(I.EQ.6) WID2=WIDS(6,3)*WIDS(8,2)
            IF(I.EQ.9) WID2=WIDS(17,2)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  320   CONTINUE
 
      ELSEIF(KFLA.EQ.42) THEN
C...LQ (leptoquark).
        FAC=(AEM/4D0)*PARU(151)*SHR
        DO 330 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 330
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 330
          WDTP(I)=FAC*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
          WID2=1D0
          ILQQ=KFDP(IDC,1)*ISIGN(1,KFLR)
          IF(ILQQ.GE.6) WID2=WIDS(ILQQ,2)
          IF(ILQQ.LE.-6) WID2=WIDS(-ILQQ,3)
          ILQL=KFDP(IDC,2)*ISIGN(1,KFLR)
          IF(ILQL.GE.17) WID2=WID2*WIDS(ILQL,2)
          IF(ILQL.LE.-17) WID2=WID2*WIDS(-ILQL,3)
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  330   CONTINUE
 
      ELSEIF(KFLA.EQ.KTECHN+111.OR.KFLA.EQ.KTECHN+221) THEN
C...Techni-pi0 and techni-pi0':
        FAC=(1D0/(32D0*PARU(1)*PARP(142)**2))*SHR
        DO 340 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 340
          PM1=PMAS(PYCOMP(KFDP(IDC,1)),1)
          PM2=PMAS(PYCOMP(KFDP(IDC,2)),1)
          RM1=PM1**2/SH
          RM2=PM2**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 340
          WID2=1D0
C...pi_tc -> g + g
          IF(I.EQ.8) THEN
            FACP=(AS/(4D0*PARU(1))*PARP(144)/PARP(142))**2
     &      /(8D0*PARU(1))*SH*SHR
            IF(KFLA.EQ.KTECHN+111) THEN
              FACP=FACP*PARP(149)
            ELSE
              FACP=FACP*PARP(150)
            ENDIF
            WDTP(I)=FACP
          ELSE
C...pi_tc -> f + fbar.
            FCOF=1D0
            IKA=IABS(KFDP(IDC,1))
            IF(IKA.LT.10) FCOF=3D0*RADC
            HM1=PM1
            HM2=PM2
            IF(IKA.GE.4.AND.IKA.LE.6) THEN
               FCOF=FCOF*PARP(141+IKA)**2
               HM1=PYMRUN(KFDP(IDC,1),SH)
               HM2=PYMRUN(KFDP(IDC,2),SH)
            ELSEIF(IKA.EQ.15) THEN
               FCOF=FCOF*PARP(148)**2
            ENDIF
            WDTP(I)=FAC*FCOF*(HM1+HM2)**2*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  340   CONTINUE
 
      ELSEIF(KFLA.EQ.KTECHN+211) THEN
C...pi+_tc
        FAC=(1D0/(32D0*PARU(1)*PARP(142)**2))*SHR
        DO 350 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 350
          PM1=PMAS(PYCOMP(KFDP(IDC,1)),1)
          PM2=PMAS(PYCOMP(KFDP(IDC,2)),1)
          PM3=0D0
          IF(I.EQ.5) PM3=PMAS(PYCOMP(KFDP(IDC,3)),1)
          RM1=PM1**2/SH
          RM2=PM2**2/SH
          RM3=PM3**2/SH
          IF(SQRT(RM1)+SQRT(RM2)+SQRT(RM3).GT.1D0) GOTO 350
          WID2=1D0
C...pi_tc -> f + f'.
          FCOF=1D0
          IF(IABS(KFDP(IDC,1)).LT.10) FCOF=3D0*RADC
C...pi_tc+ -> W b b~
          IF(I.EQ.5.AND.SHR.LT.PMAS(6,1)+PMAS(5,1)) THEN
            FCOF=3D0*RADC
            XMT2=PMAS(6,1)**2/SH
            FACP=FAC/(4D0*PARU(1))*FCOF*XMT2*PARP(147)**2
            KFC3=PYCOMP(KFDP(IDC,3))
            CHECK = SQRT(RM1)+SQRT(RM2)+SQRT(RM3)
            CHECK = SQRT(RM1)
            T0 = (1D0-CHECK**2)*
     &      (XMT2*(6D0*XMT2**2+3D0*XMT2*RM1-4D0*RM1**2)-
     &      (5D0*XMT2**2+2D0*XMT2*RM1-8D0*RM1**2))/(4D0*XMT2**2)
            T1 = (1D0-XMT2)*(RM1-XMT2)*((XMT2**2+XMT2*RM1+4D0*RM1**2)
     &      -3D0*XMT2**2*(XMT2+RM1))/(2D0*XMT2**3)
            T3 = RM1**2/XMT2**3*(3D0*XMT2-4D0*RM1+4D0*XMT2*RM1)
            WDTP(I)=FACP*(T0 + T1*LOG((XMT2-CHECK**2)/(XMT2-1D0))
     &      +T3*LOG(CHECK))
            IF(KFLR.GT.0) THEN
               WID2=WIDS(24,2)
            ELSE
               WID2=WIDS(24,3)
            ENDIF
          ELSE
            FCOF=1D0
            IKA=IABS(KFDP(IDC,1))
            IF(IKA.LT.10) FCOF=3D0*RADC
            HM1=PM1
            HM2=PM2
            IF(I.GE.1.AND.I.LE.5) THEN
              IF(I.LE.2) THEN
                FCOF=FCOF*PARP(145)**2
              ELSEIF(I.LE.4) THEN
                FCOF=FCOF*PARP(146)**2
              ELSEIF(I.EQ.5) THEN
                FCOF=FCOF*PARP(147)**2
              ENDIF
              HM1=PYMRUN(KFDP(IDC,1),SH)
              HM2=PYMRUN(KFDP(IDC,2),SH)
            ELSEIF(I.EQ.8) THEN
              FCOF=FCOF*PARP(148)**2
            ENDIF
            WDTP(I)=FAC*FCOF*(HM1+HM2)**2*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  350     CONTINUE
 
      ELSEIF(KFLA.EQ.KTECHN+331) THEN
C...Techni-eta.
        FAC=(SH/PARP(46)**2)*SHR
        DO 360 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 360
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 360
          WID2=1D0
          IF(I.LE.2) THEN
            WDTP(I)=FAC*RM1*SQRT(MAX(0D0,1D0-4D0*RM1))/(4D0*PARU(1))
            IF(I.EQ.2) WID2=WIDS(6,1)
          ELSE
            WDTP(I)=FAC*5D0*AS**2/(96D0*PARU(1)**3)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  360   CONTINUE
 
      ELSEIF(KFLA.EQ.KTECHN+113) THEN
C...Techni-rho0:
        ALPRHT=2.91D0*(3D0/PARP(144))
        FAC=(ALPRHT/12D0)*SHR
        FACF=(1D0/6D0)*(AEM**2/ALPRHT)*SHR
        SQMZ=PMAS(23,1)**2
        SQMW=PMAS(24,1)**2
        SHP=SH
        CALL PYWIDX(23,SHP,WDTPP,WDTEP)
        GMMZ=SHR*WDTPP(0)
        XWRHT=(1D0-2D0*XW)/(4D0*XW*(1D0-XW))
        BWZR=XWRHT*SH*(SH-SQMZ)/((SH-SQMZ)**2+GMMZ**2)
        BWZI=XWRHT*SH*GMMZ/((SH-SQMZ)**2+GMMZ**2)
        DO 370 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 370
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 370
          WID2=1D0
          IF(I.EQ.1) THEN
C...rho_tc0 -> W+ + W-.
            WDTP(I)=FAC*PARP(141)**4*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
            WID2=WIDS(24,1)
          ELSEIF(I.EQ.2) THEN
C...rho_tc0 -> W+ + pi_tc-.
            WDTP(I)=FAC*PARP(141)**2*(1D0-PARP(141)**2)*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3+
     &      AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0-RM1-RM2)**2-4D0*RM1*RM2 + 6D0*SQMW/SH)*
     &      (1D0-PARP(141)**2)/4D0/XW/24D0/PARP(138)**2*SHR**3
            WID2=WIDS(24,2)*WIDS(PYCOMP(KTECHN+211),3)
          ELSEIF(I.EQ.3) THEN
C...rho_tc0 -> pi_tc+ + W-.
            WDTP(I)=FAC*PARP(141)**2*(1D0-PARP(141)**2)*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3+
     &      AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0-RM1-RM2)**2-4D0*RM1*RM2 + 6D0*SQMW/SH)*
     &      (1D0-PARP(141)**2)/4D0/XW/24D0/PARP(138)**2*SHR**3
            WID2=WIDS(PYCOMP(KTECHN+211),2)*WIDS(24,3)
          ELSEIF(I.EQ.4) THEN
C...rho_tc0 -> pi_tc+ + pi_tc-.
            WDTP(I)=FAC*(1D0-PARP(141)**2)**2*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
            WID2=WIDS(PYCOMP(KTECHN+211),1)
          ELSEIF(I.EQ.5) THEN
C...rho_tc0 -> gamma + pi_tc0
            WDTP(I)=AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (2D0*PARP(143)-1D0)**2*(1D0-PARP(141)**2)/24D0/PARP(137)**2*
     &      SHR**3
            WID2=WIDS(PYCOMP(KTECHN+111),2)
          ELSEIF(I.EQ.6) THEN
C...rho_tc0 -> gamma + pi_tc0'
            WDTP(I)=AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (1D0-PARP(139)**2)/24D0/PARP(137)**2*SHR**3
            WID2=WIDS(PYCOMP(KTECHN+221),2)
          ELSEIF(I.EQ.7) THEN
C...rho_tc0 -> Z0 + pi_tc0
            WDTP(I)=AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (2D0*PARP(143)-1D0)**2*(1D0-PARP(141)**2)/24D0/PARP(137)**2*
     &      XW/XW1*SHR**3
            WID2=WIDS(23,2)*WIDS(PYCOMP(KTECHN+111),2)
          ELSEIF(I.EQ.8) THEN
C...rho_tc0 -> Z0 + pi_tc0'
            WDTP(I)=AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (1D0-PARP(139)**2)/24D0/PARP(137)**2*(1D0-2D0*XW)**2/4D0/
     &      XW/XW1*SHR**3
            WID2=WIDS(23,2)*WIDS(PYCOMP(KTECHN+221),2)
          ELSE
C...rho_tc0 -> f + fbar.
            WID2=1D0
            IF(I.LE.16) THEN
              IA=I-8
              FCOF=3D0*RADC
              IF(IA.GE.6.AND.IA.LE.8) WID2=WIDS(IA,1)
            ELSE
              IA=I-6
              FCOF=1D0
              IF(IA.GE.17) WID2=WIDS(IA,1)
            ENDIF
            EI=KCHG(IA,1)/3D0
            AI=SIGN(1D0,EI+0.1D0)
            VI=AI-4D0*EI*XWV
            VALI=0.5D0*(VI+AI)
            VARI=0.5D0*(VI-AI)
            WDTP(I)=FACF*FCOF*SQRT(MAX(0D0,1D0-4D0*RM1))*((1D0-RM1)*
     &      ((EI+VALI*BWZR)**2+(VALI*BWZI)**2+
     &      (EI+VARI*BWZR)**2+(VARI*BWZI)**2)+6D0*RM1*(
     &      (EI+VALI*BWZR)*(EI+VARI*BWZR)+VALI*VARI*BWZI**2))
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  370   CONTINUE
 
      ELSEIF(KFLA.EQ.KTECHN+213) THEN
C...Techni-rho+/-:
        ALPRHT=2.91D0*(3D0/PARP(144))
        FAC=(ALPRHT/12D0)*SHR
        SQMZ=PMAS(23,1)**2
        SQMW=PMAS(24,1)**2
        SHP=SH
        CALL PYWIDX(24,SHP,WDTPP,WDTEP)
        GMMW=SHR*WDTPP(0)
        FACF=(1D0/12D0)*(AEM**2/ALPRHT)*SHR*
     &  (0.125D0/XW**2)*SH**2/((SH-SQMW)**2+GMMW**2)
        DO 380 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 380
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 380
          WID2=1D0
          IF(I.EQ.1) THEN
C...rho_tc+ -> W+ + Z0.
            WDTP(I)=FAC*PARP(141)**4*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,2)*WIDS(23,2)
            ELSE
              WID2=WIDS(24,3)*WIDS(23,2)
            ENDIF
          ELSEIF(I.EQ.2) THEN
C...rho_tc+ -> W+ + pi_tc0.
            WDTP(I)=FAC*PARP(141)**2*(1D0-PARP(141)**2)*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3+
     &      AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0-RM1-RM2)**2-4D0*RM1*RM2 + 6D0*SQMW/SH)*
     &      (1D0-PARP(141)**2)/4D0/XW/24D0/PARP(138)**2*SHR**3
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,2)*WIDS(PYCOMP(KTECHN+111),2)
            ELSE
              WID2=WIDS(24,3)*WIDS(PYCOMP(KTECHN+111),2)
            ENDIF
          ELSEIF(I.EQ.3) THEN
C...rho_tc+ -> pi_tc+ + Z0.
            WDTP(I)=FAC*PARP(141)**2*(1D0-PARP(141)**2)*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3+
     &      AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))*
     &      ((1D0-RM1-RM2)**2-4D0*RM1*RM2 + 6D0*SQMZ/SH)*
     &      (1D0-PARP(141)**2)/4D0/XW/XW1/24D0/PARP(138)**2*SHR**3+
     &      AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (2D0*PARP(143)-1D0)**2*(1D0-PARP(141)**2)/24D0/PARP(137)**2*
     &      SHR**3*XW/XW1
            IF(KFLR.GT.0) THEN
              WID2=WIDS(PYCOMP(KTECHN+211),2)*WIDS(23,2)
            ELSE
              WID2=WIDS(PYCOMP(KTECHN+211),3)*WIDS(23,2)
            ENDIF
          ELSEIF(I.EQ.4) THEN
C...rho_tc+ -> pi_tc+ + pi_tc0.
            WDTP(I)=FAC*(1D0-PARP(141)**2)**2*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
            IF(KFLR.GT.0) THEN
              WID2=WIDS(PYCOMP(KTECHN+211),2)*WIDS(PYCOMP(KTECHN+111),2)
            ELSE
              WID2=WIDS(PYCOMP(KTECHN+211),3)*WIDS(PYCOMP(KTECHN+111),2)
            ENDIF
          ELSEIF(I.EQ.5) THEN
C...rho_tc+ -> pi_tc+ + gamma
            WDTP(I)=AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (2D0*PARP(143)-1D0)**2*(1D0-PARP(141)**2)/24D0/PARP(137)**2*
     &      SHR**3
            IF(KFLR.GT.0) THEN
              WID2=WIDS(PYCOMP(KTECHN+211),2)
            ELSE
              WID2=WIDS(PYCOMP(KTECHN+211),3)
            ENDIF
          ELSEIF(I.EQ.6) THEN
C...rho_tc+ -> W+ + pi_tc0'
            WDTP(I)=AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (1D0-PARP(139)**2)/4D0/XW/24D0/PARP(137)**2*SHR**3
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,2)*WIDS(PYCOMP(KTECHN+221),2)
            ELSE
              WID2=WIDS(24,3)*WIDS(PYCOMP(KTECHN+221),2)
            ENDIF
          ELSE
C...rho_tc+ -> f + fbar'.
            IA=I-6
            WID2=1D0
            IF(IA.LE.16) THEN
              FCOF=3D0*RADC*VCKM((IA-1)/4+1,MOD(IA-1,4)+1)
              IF(KFLR.GT.0) THEN
                IF(MOD(IA,4).EQ.3) WID2=WIDS(6,2)
                IF(MOD(IA,4).EQ.0) WID2=WIDS(8,2)
                IF(IA.GE.13) WID2=WID2*WIDS(7,3)
              ELSE
                IF(MOD(IA,4).EQ.3) WID2=WIDS(6,3)
                IF(MOD(IA,4).EQ.0) WID2=WIDS(8,3)
                IF(IA.GE.13) WID2=WID2*WIDS(7,2)
              ENDIF
            ELSE
              FCOF=1D0
              IF(KFLR.GT.0) THEN
                IF(IA.EQ.20) WID2=WIDS(17,3)*WIDS(18,2)
              ELSE
                IF(IA.EQ.20) WID2=WIDS(17,2)*WIDS(18,3)
              ENDIF
            ENDIF
            WDTP(I)=FACF*FCOF*(2D0-RM1-RM2-(RM1-RM2)**2)*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  380   CONTINUE
 
      ELSEIF(KFLA.EQ.KTECHN+223) THEN
C...Techni-omega:
        ALPRHT=2.91D0*(3D0/PARP(144))
        FAC=(ALPRHT/12D0)*SHR
        FACF=(1D0/6D0)*(AEM**2/ALPRHT)*SHR*(2D0*PARP(143)-1D0)**2
        SQMZ=PMAS(23,1)**2
        SHP=SH
        CALL PYWIDX(23,SHP,WDTPP,WDTEP)
        GMMZ=SHR*WDTPP(0)
        BWZR=(0.5D0/(1D0-XW))*SH*(SH-SQMZ)/((SH-SQMZ)**2+GMMZ**2)
        BWZI=-(0.5D0/(1D0-XW))*SH*GMMZ/((SH-SQMZ)**2+GMMZ**2)
        DO 390 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 390
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 390
          WID2=1D0
          IF(I.EQ.1) THEN
C...omega_tc0 -> gamma + pi_tc0.
            WDTP(I)=AEM/24D0/PARP(137)**2*(1D0-PARP(141)**2)*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*SHR**3
            WID2=WIDS(PYCOMP(KTECHN+111),2)
          ELSEIF(I.EQ.2) THEN
C...omega_tc0 -> Z0 + pi_tc0
            WDTP(I)=AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (1D0-PARP(141)**2)/24D0/PARP(137)**2*(1D0-2D0*XW)**2/4D0/
     &      XW/XW1*SHR**3
            WID2=WIDS(23,2)*WIDS(PYCOMP(KTECHN+111),2)
          ELSEIF(I.EQ.3) THEN
C...omega_tc0 -> gamma + pi_tc0'
            WDTP(I)=AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (2D0*PARP(143)-1D0)**2*(1D0-PARP(139)**2)/24D0/PARP(137)**2*
     &      SHR**3
            WID2=WIDS(PYCOMP(KTECHN+221),2)
          ELSEIF(I.EQ.4) THEN
C...omega_tc0 -> Z0 + pi_tc0'
            WDTP(I)=AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (2D0*PARP(143)-1D0)**2*(1D0-PARP(139)**2)/24D0/PARP(137)**2*
     &      XW/XW1*SHR**3
            WID2=WIDS(23,2)*WIDS(PYCOMP(KTECHN+221),2)
          ELSEIF(I.EQ.5) THEN
C...omega_tc0 -> W+ + pi_tc-
            WDTP(I)=AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (1D0-PARP(141)**2)/4D0/XW/24D0/PARP(137)**2*SHR**3+
     &      FAC*PARP(141)**2*(1D0-PARP(141)**2)*PARP(140)**2*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
            WID2=WIDS(24,2)*WIDS(PYCOMP(KTECHN+211),3)
          ELSEIF(I.EQ.6) THEN
C...omega_tc0 -> pi_tc+ + W-
            WDTP(I)=AEM*SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3*
     &      (1D0-PARP(141)**2)/4D0/XW/24D0/PARP(137)**2*SHR**3+
     &      FAC*PARP(141)**2*(1D0-PARP(141)**2)*PARP(140)**2*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
            WID2=WIDS(24,3)*WIDS(PYCOMP(KTECHN+211),2)
          ELSEIF(I.EQ.7) THEN
C...omega_tc0 -> W+ + W-.
            WDTP(I)=FAC*PARP(141)**4*PARP(140)**2*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
            WID2=WIDS(24,1)
          ELSEIF(I.EQ.8) THEN
C...omega_tc0 -> pi_tc+ + pi_tc-.
            WDTP(I)=FAC*(1D0-PARP(141)**2)**2*PARP(140)**2*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))**3
            WID2=WIDS(PYCOMP(KTECHN+211),1)
          ELSE
C...omega_tc0 -> f + fbar.
            WID2=1D0
            IF(I.LE.14) THEN
              IA=I-8
              FCOF=3D0*RADC
              IF(IA.GE.6.AND.IA.LE.8) WID2=WIDS(IA,1)
            ELSE
              IA=I-6
              FCOF=1D0
              IF(IA.GE.17) WID2=WIDS(IA,1)
            ENDIF
            EI=KCHG(IA,1)/3D0
            AI=SIGN(1D0,EI+0.1D0)
            VI=AI-4D0*EI*XWV
            VALI=-0.5D0*(VI+AI)
            VARI=-0.5D0*(VI-AI)
            WDTP(I)=FACF*FCOF*SQRT(MAX(0D0,1D0-4D0*RM1))*((1D0-RM1)*
     &      ((EI+VALI*BWZR)**2+(VALI*BWZI)**2+
     &      (EI+VARI*BWZR)**2+(VARI*BWZI)**2)+6D0*RM1*(
     &      (EI+VALI*BWZR)*(EI+VARI*BWZR)+VALI*VARI*BWZI**2))
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  390   CONTINUE
 
C.....V8 -> quark anti-quark
      ELSEIF(KFLA.EQ.KTECHN+100021) THEN
        FAC=AS/6D0*SHR
        TANT3=ABS(PARP(155))
        IF(PARP(155).GT.0) THEN
          IMDL=1
        ELSE
          IMDL=2
        ENDIF
        DO 400 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 400
          PM1=PMAS(PYCOMP(KFDP(IDC,1)),1)
          RM1=PM1**2/SH
          IF(RM1.GT.0.25D0) GOTO 400
          WID2=1D0
          IF(I.EQ.5.OR.I.EQ.6.OR.IMDL.EQ.2) THEN
            FMIX=1D0/TANT3**2
          ELSE
            FMIX=TANT3**2
          ENDIF
          WDTP(I)=FAC*(1D0+2D0*RM1)*SQRT(1D0-4D0*RM1)*FMIX
          IF(I.EQ.6) WID2=WIDS(6,1)
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  400   CONTINUE
 
      ELSEIF(KFLA.EQ.KTECHN+100111.OR.KFLA.EQ.KTECHN+200111) THEN
        FAC=(1D0/(4D0*PARU(1)*PARP(142)**2))*SHR
        CLEBF=0D0
        DO 410 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 410
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 410
          WID2=1D0
C...pi_tc -> g + g
          IF(I.EQ.7) THEN
            IF(KFLA.EQ.KTECHN+100111) THEN
              CLEBG=4D0/3D0
            ELSE
              CLEBG=5D0/3D0
            ENDIF
            FACP=(AS/(8D0*PARU(1))*PARP(144)/PARP(142))**2
     &      /(2D0*PARU(1))*SH*SHR*CLEBG
            WDTP(I)=FACP
          ELSE
C...pi_tc -> f + fbar.
            IF(I.EQ.6) WID2=WIDS(6,1)
            FCOF=1D0
            IKA=IABS(KFDP(IDC,1))
            IF(IKA.LT.10) FCOF=3D0*RADC
            HM1=PYMRUN(KFDP(IDC,1),SH)
            WDTP(I)=FAC*FCOF*HM1**2*CLEBF*
     &      SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  410   CONTINUE
 
      ELSEIF(KFLA.GE.KTECHN+100113.AND.KFLA.LE.KTECHN+400113) THEN
        FAC=AS/6D0*SHR
        ALPRHT=2.91D0*(3D0/PARP(144))
        TANT3=ABS(PARP(155))
        SIN2T=2D0*TANT3/(TANT3**2+1D0)
        SINT3=TANT3/SQRT(TANT3**2+1D0)
        CSXPP=1D0/SQRT(2D0)
        RM82=PARP(156)**2
        X12=(1D0/SQRT(2D0)*1D0/SQRT(2D0)+
     &  1D0/SQRT(2D0)*1D0/SQRT(2D0))/SQRT(2D0)
        X21=1D-6
        X11=(.25D0*((1D0/SQRT(2D0))**2+(1D0/SQRT(2D0))**2+2D0)-
     &  SINT3**2)*2D0
        X22=(.25D0*((1D0/SQRT(2D0))**2+(1D0/SQRT(2D0))**2)-
     &  SINT3**2)*2D0
        IF(PARP(155).GT.0) THEN
          IMDL=1
        ELSE
          IMDL=2
        ENDIF
        DO 420 I=1,MDCY(KC,3)
          IF(I.EQ.7.AND.(KFLA.EQ.KTECHN+200113.OR.
     &    KFLA.EQ.KTECHN+300113)) GOTO 420
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 420
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 420
          WID2=1D0
          IF(I.LE.6) THEN
            IF(I.EQ.6) WID2=WIDS(6,1)
            XIG=1D0
            IF(KFLA.EQ.KTECHN+200113) THEN
              XIG=0D0
              XIJ=X12
            ELSEIF(KFLA.EQ.KTECHN+300113) THEN
              XIG=0D0
              XIJ=X21
            ELSEIF(KFLA.EQ.KTECHN+100113) THEN
              XIJ=X11
            ELSE
              XIJ=X22
            ENDIF
            IF(I.EQ.5.OR.I.EQ.6.OR.IMDL.EQ.2) THEN
              FMIX=1D0/TANT3/SIN2T
            ELSE
              FMIX=-TANT3/SIN2T
            ENDIF
            XFAC=(XIG+FMIX*XIJ)**2
            WDTP(I)=FAC*(1D0+2D0*RM1)*SQRT(1D0-4D0*RM1)*AS/ALPRHT*XFAC
          ELSEIF(I.EQ.7) THEN
            WDTP(I)=SHR*AS**2/(2D0*ALPRHT)
          ELSEIF(KFLA.EQ.KTECHN+400113.AND.I.LE.9) THEN
            PSH=SHR*(1D0-RM1)/2D0
            WDTP(I)=AS/9D0*PSH**3/RM82
            IF(I.EQ.8) THEN
              WDTP(I)=2D0*WDTP(I)*CSXPP**2
              WID2=WIDS(PYCOMP(KFDP(IDC,1)),2)
            ELSE
              WDTP(I)=5D0*WDTP(I)
              WID2=WIDS(PYCOMP(KFDP(IDC,1)),2)
            ENDIF
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  420   CONTINUE
 
      ELSEIF(KFLA.EQ.KEXCIT+1) THEN
C...d* excited quark.
        FAC=(SH/PARU(155)**2)*SHR
        DO 430 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 430
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 430
          WID2=1D0
          IF(I.EQ.1) THEN
C...d* -> g + d.
            WDTP(I)=FAC*AS*PARU(159)**2/3D0
            WID2=1D0
          ELSEIF(I.EQ.2) THEN
C...d* -> gamma + d.
            QF=-PARU(157)/2D0+PARU(158)/6D0
            WDTP(I)=FAC*AEM*QF**2/4D0
            WID2=1D0
          ELSEIF(I.EQ.3) THEN
C...d* -> Z0 + d.
            QF=-PARU(157)*XW1/2D0-PARU(158)*XW/6D0
            WDTP(I)=FAC*AEM*QF**2/(8D0*XW*XW1)*
     &      (1D0-RM1)**2*(2D0+RM1)
            WID2=WIDS(23,2)
          ELSEIF(I.EQ.4) THEN
C...d* -> W- + u.
            WDTP(I)=FAC*AEM*PARU(157)**2/(16D0*XW)*
     &      (1D0-RM1)**2*(2D0+RM1)
            IF(KFLR.GT.0) WID2=WIDS(24,3)
            IF(KFLR.LT.0) WID2=WIDS(24,2)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  430   CONTINUE
 
      ELSEIF(KFLA.EQ.KEXCIT+2) THEN
C...u* excited quark.
        FAC=(SH/PARU(155)**2)*SHR
        DO 440 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 440
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 440
          WID2=1D0
          IF(I.EQ.1) THEN
C...u* -> g + u.
            WDTP(I)=FAC*AS*PARU(159)**2/3D0
            WID2=1D0
          ELSEIF(I.EQ.2) THEN
C...u* -> gamma + u.
            QF=PARU(157)/2D0+PARU(158)/6D0
            WDTP(I)=FAC*AEM*QF**2/4D0
            WID2=1D0
          ELSEIF(I.EQ.3) THEN
C...u* -> Z0 + u.
            QF=PARU(157)*XW1/2D0-PARU(158)*XW/6D0
            WDTP(I)=FAC*AEM*QF**2/(8D0*XW*XW1)*
     &      (1D0-RM1)**2*(2D0+RM1)
            WID2=WIDS(23,2)
          ELSEIF(I.EQ.4) THEN
C...u* -> W+ + d.
            WDTP(I)=FAC*AEM*PARU(157)**2/(16D0*XW)*
     &      (1D0-RM1)**2*(2D0+RM1)
            IF(KFLR.GT.0) WID2=WIDS(24,2)
            IF(KFLR.LT.0) WID2=WIDS(24,3)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  440   CONTINUE
 
      ELSEIF(KFLA.EQ.KEXCIT+11) THEN
C...e* excited lepton.
        FAC=(SH/PARU(155)**2)*SHR
        DO 450 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 450
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 450
          WID2=1D0
          IF(I.EQ.1) THEN
C...e* -> gamma + e.
            QF=-PARU(157)/2D0-PARU(158)/2D0
            WDTP(I)=FAC*AEM*QF**2/4D0
            WID2=1D0
          ELSEIF(I.EQ.2) THEN
C...e* -> Z0 + e.
            QF=-PARU(157)*XW1/2D0+PARU(158)*XW/2D0
            WDTP(I)=FAC*AEM*QF**2/(8D0*XW*XW1)*
     &      (1D0-RM1)**2*(2D0+RM1)
            WID2=WIDS(23,2)
          ELSEIF(I.EQ.3) THEN
C...e* -> W- + nu.
            WDTP(I)=FAC*AEM*PARU(157)**2/(16D0*XW)*
     &      (1D0-RM1)**2*(2D0+RM1)
            IF(KFLR.GT.0) WID2=WIDS(24,3)
            IF(KFLR.LT.0) WID2=WIDS(24,2)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  450   CONTINUE
 
      ELSEIF(KFLA.EQ.KEXCIT+12) THEN
C...nu*_e excited neutrino.
        FAC=(SH/PARU(155)**2)*SHR
        DO 460 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 460
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 460
          WID2=1D0
          IF(I.EQ.1) THEN
C...nu*_e -> Z0 + nu*_e.
            QF=PARU(157)*XW1/2D0+PARU(158)*XW/2D0
            WDTP(I)=FAC*AEM*QF**2/(8D0*XW*XW1)*
     &      (1D0-RM1)**2*(2D0+RM1)
            WID2=WIDS(23,2)
          ELSEIF(I.EQ.2) THEN
C...nu*_e -> W+ + e.
            WDTP(I)=FAC*AEM*PARU(157)**2/(16D0*XW)*
     &      (1D0-RM1)**2*(2D0+RM1)
            IF(KFLR.GT.0) WID2=WIDS(24,2)
            IF(KFLR.LT.0) WID2=WIDS(24,3)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  460   CONTINUE
 
      ELSEIF(KFLA.EQ.KDIMEN+39) THEN
C...G* (graviton resonance):
        FAC=(PARP(50)**2/PARU(1))*SHR
        DO 470 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 470
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 470
          WID2=1D0
          IF(I.LE.8) THEN
C...G* -> q + qbar
            FCOF=3D0*RADC
            IF(I.GE.6.AND.MSTP(35).GE.1) FCOF=FCOF*
     &      PYHFTH(SH,SH*RM1,1D0)
            WDTP(I)=FAC*FCOF*SQRT(MAX(0D0,1D0-4D0*RM1))**3*
     &      (1D0+8D0*RM1/3D0)/320D0
            IF(I.EQ.6) WID2=WIDS(6,1)
            IF(I.EQ.7.OR.I.EQ.8) WID2=WIDS(I,1)
          ELSEIF(I.LE.16) THEN
C...G* -> l+ + l-, nu + nubar
            FCOF=1D0
            WDTP(I)=FAC*SQRT(MAX(0D0,1D0-4D0*RM1))**3*
     &      (1D0+8D0*RM1/3D0)/320D0
            IF(I.EQ.15.OR.I.EQ.16) WID2=WIDS(2+I,1)
          ELSEIF(I.EQ.17) THEN
C...G* -> g + g.
            WDTP(I)=FAC/20D0
          ELSEIF(I.EQ.18) THEN
C...G* -> gamma + gamma.
            WDTP(I)=FAC/160D0
          ELSEIF(I.EQ.19) THEN
C...G* -> Z0 + Z0.
            WDTP(I)=FAC*SQRT(MAX(0D0,1D0-4D0*RM1))*(13D0/12D0+
     &      14D0*RM1/3D0+4D0*RM1**2)/160D0
            WID2=WIDS(23,1)
          ELSEIF(I.EQ.20) THEN
C...G* -> W+ + W-.
            WDTP(I)=FAC*SQRT(MAX(0D0,1D0-4D0*RM1))*(13D0/12D0+
     &      14D0*RM1/3D0+4D0*RM1**2)/80D0
            WID2=WIDS(24,1)
          ENDIF
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  470   CONTINUE
 
      ELSEIF(KFLA.EQ.9900012.OR.KFLA.EQ.9900014.OR.KFLA.EQ.9900016) THEN
C...nu_eR, nu_muR, nu_tauR: righthanded Majorana neutrinos.
        PMWR=MAX(1.001D0*SHR,PMAS(PYCOMP(9900024),1))
        FAC=(AEM**2/(768D0*PARU(1)*XW**2))*SHR**5/PMWR**4
        DO 480 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 480
          PM1=PMAS(PYCOMP(KFDP(IDC,1)),1)
          PM2=PMAS(PYCOMP(KFDP(IDC,2)),1)
          PM3=PMAS(PYCOMP(KFDP(IDC,3)),1)
          IF(PM1+PM2+PM3.GE.SHR) GOTO 480
          WID2=1D0
          IF(I.LE.9) THEN
C...nu_lR -> l- qbar q'
            FCOF=3D0*RADC*VCKM((I-1)/3+1,MOD(I-1,3)+1)
            IF(MOD(I,3).EQ.0) WID2=WIDS(6,2)
          ELSEIF(I.LE.18) THEN
C...nu_lR -> l+ q qbar'
            FCOF=3D0*RADC*VCKM((I-10)/3+1,MOD(I-10,3)+1)
            IF(MOD(I-9,3).EQ.0) WID2=WIDS(6,3)
          ELSE
C...nu_lR -> l- l'+ nu_lR' + charge conjugate.
            FCOF=1D0
            WID2=WIDS(PYCOMP(KFDP(IDC,3)),2)
          ENDIF
          X=(PM1+PM2+PM3)/SHR
          FX=1D0-8D0*X**2+8D0*X**6-X**8-24D0*X**4*LOG(X)
          Y=(SHR/PMWR)**2
          FY=(12D0*(1D0-Y)*LOG(1D0-Y)+12D0*Y-6D0*Y**2-2D0*Y**3)/Y**4
          WDTP(I)=FAC*FCOF*FX*FY
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  480   CONTINUE
 
      ELSEIF(KFLA.EQ.9900023) THEN
C...Z_R0:
        FAC=(AEM/(48D0*XW*XW1*(1D0-2D0*XW)))*SHR
        DO 490 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 490
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 490
          WID2=1D0
          SYMMET=1D0
          IF(I.LE.6) THEN
C...Z_R0 -> q + qbar
            EF=KCHG(I,1)/3D0
            AF=SIGN(1D0,EF+0.1D0)*(1D0-2D0*XW)
            VF=SIGN(1D0,EF+0.1D0)-4D0*EF*XW
            FCOF=3D0*RADC
            IF(I.EQ.6) WID2=WIDS(6,1)
          ELSEIF(I.EQ.7.OR.I.EQ.10.OR.I.EQ.13) THEN
C...Z_R0 -> l+ + l-
            AF=-(1D0-2D0*XW)
            VF=-1D0+4D0*XW
            FCOF=1D0
          ELSEIF(I.EQ.8.OR.I.EQ.11.OR.I.EQ.14) THEN
C...Z0 -> nu_L + nu_Lbar, assumed Majorana.
            AF=-2D0*XW
            VF=0D0
            FCOF=1D0
            SYMMET=0.5D0
          ELSEIF(I.LE.15) THEN
C...Z0 -> nu_R + nu_R, assumed Majorana.
            AF=2D0*XW1
            VF=0D0
            FCOF=1D0
            WID2=WIDS(PYCOMP(KFDP(IDC,1)),1)
            SYMMET=0.5D0
          ENDIF
          WDTP(I)=FAC*FCOF*(VF**2*(1D0+2D0*RM1)+AF**2*(1D0-4D0*RM1))*
     &    SQRT(MAX(0D0,1D0-4D0*RM1))*SYMMET
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  490   CONTINUE
 
      ELSEIF(KFLA.EQ.9900024) THEN
C...W_R+/-:
        FAC=(AEM/(24D0*XW))*SHR
        DO 500 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 500
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 500
          WID2=1D0
          IF(I.LE.9) THEN
C...W_R+/- -> q + qbar'
            FCOF=3D0*RADC*VCKM((I-1)/3+1,MOD(I-1,3)+1)
            IF(KFLR.GT.0) THEN
              IF(MOD(I,3).EQ.0) WID2=WIDS(6,2)
            ELSE
              IF(MOD(I,3).EQ.0) WID2=WIDS(6,3)
            ENDIF
          ELSEIF(I.LE.12) THEN
C...W_R+/- -> l+/- + nu_R
            FCOF=1D0
          ENDIF
          WDTP(I)=FAC*FCOF*(2D0-RM1-RM2-(RM1-RM2)**2)*
     &    SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  500  CONTINUE
 
      ELSEIF(KFLA.EQ.9900041) THEN
C...H_L++/--:
        FAC=(1D0/(8D0*PARU(1)))*SHR
        DO 510 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 510
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 510
          WID2=1D0
          IF(I.LE.6) THEN
C...H_L++/-- -> l+/- + l'+/-
            FCOF=PARP(180+3*((IABS(KFDP(IDC,1))-11)/2)+
     &      (IABS(KFDP(IDC,2))-9)/2)**2
            IF(KFDP(IDC,1).NE.KFDP(IDC,2)) FCOF=2D0*FCOF
          ELSEIF(I.EQ.7) THEN
C...H_L++/-- -> W_L+/- + W_L+/-
            FCOF=0.5D0*PARP(190)**4*PARP(192)**2/PMAS(24,1)**2*
     &      (3D0*RM1+0.25D0/RM1-1D0)
            WID2=WIDS(24,4+(1-KFLS)/2)
          ENDIF
          WDTP(I)=FAC*FCOF*
     &    SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  510   CONTINUE
 
      ELSEIF(KFLA.EQ.9900042) THEN
C...H_R++/--:
        FAC=(1D0/(8D0*PARU(1)))*SHR
        DO 520 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 520
          RM1=PMAS(PYCOMP(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(PYCOMP(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 520
          WID2=1D0
          IF(I.LE.6) THEN
C...H_R++/-- -> l+/- + l'+/-
            FCOF=PARP(180+3*((IABS(KFDP(IDC,1))-11)/2)+
     &      (IABS(KFDP(IDC,2))-9)/2)**2
            IF(KFDP(IDC,1).NE.KFDP(IDC,2)) FCOF=2D0*FCOF
          ELSEIF(I.EQ.7) THEN
C...H_R++/-- -> W_R+/- + W_R+/-
            FCOF=PARP(191)**2*(3D0*RM1+0.25D0/RM1-1D0)
            WID2=WIDS(PYCOMP(9900024),4+(1-KFLS)/2)
          ENDIF
          WDTP(I)=FAC*FCOF*
     &    SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
          WDTP(I)=FUDGE*WDTP(I)
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  520  CONTINUE
 
      ENDIF
      MINT(61)=0
      MINT(62)=0
      MINT(63)=0
      RETURN
      END
