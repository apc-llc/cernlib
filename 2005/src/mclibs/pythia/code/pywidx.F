 
C***********************************************************************
 
C...PYWIDX
C...Calculates full and partial widths of resonances.
C....copy of PYWIDT, used for techniparticle widths
 
      SUBROUTINE PYWIDX(KFLR,SH,WDTP,WDTE)
 
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
      SAVE /PYDAT1/,/PYDAT2/,/PYDAT3/,/PYSUBS/,/PYPARS/,/PYINT1/,
     &/PYINT4/,/PYMSSM/
C...Local arrays and saved variables.
      DIMENSION WDTP(0:400),WDTE(0:400,0:5),MOFSV(3,2),WIDWSV(3,2),
     &WID2SV(3,2)
      SAVE MOFSV,WIDWSV,WID2SV
      DATA MOFSV/6*0/,WIDWSV/6*0D0/,WID2SV/6*0D0/
 
C...Compressed code and sign; mass.
      KFLA=IABS(KFLR)
      KFLS=ISIGN(1,KFLR)
      KC=PYCOMP(KFLA)
      SHR=SQRT(SH)
      PMR=PMAS(KC,1)
 
C...Reset width information.
      DO 110 I=0,200
        WDTP(I)=0D0
        DO 100 J=0,5
          WDTE(I,J)=0D0
  100   CONTINUE
  110 CONTINUE
 
C...Common electroweak and strong constants.
      XW=PARU(102)
      XWV=XW
      IF(MSTP(8).GE.2) XW=1D0-(PMAS(24,1)/PMAS(23,1))**2
      XW1=1D0-XW
      AEM=PYALEM(SH)
      IF(MSTP(8).GE.1) AEM=SQRT(2D0)*PARU(105)*PMAS(24,1)**2*XW/PARU(1)
      AS=PYALPS(SH)
      RADC=1D0+AS/PARU(1)
 
      IF(KFLA.EQ.23) THEN
C...Z0:
        ICASE=1
        XWC=1D0/(16D0*XW*XW1)
        FAC=(AEM*XWC/3D0)*SHR
  120   CONTINUE
        DO 130 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 130
          RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 130
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
            WDTP(I)=FAC*FCOF*(VF**2*(1D0+2D0*RM1)+AF**2*(1D0-4D0*RM1))*
     &      BE34
            WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
              WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
              WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+
     &        WDTE(I,MDME(IDC,1))
              WDTE(I,0)=WDTE(I,MDME(IDC,1))
              WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  130   CONTINUE
 
 
      ELSEIF(KFLA.EQ.24) THEN
C...W+/-:
        FAC=(AEM/(24D0*XW))*SHR
        DO 140 I=1,MDCY(KC,3)
          IDC=I+MDCY(KC,2)-1
          IF(MDME(IDC,1).LT.0) GOTO 140
          RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
          RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
          IF(SQRT(RM1)+SQRT(RM2).GT.1D0) GOTO 140
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
          WDTP(0)=WDTP(0)+WDTP(I)
          IF(MDME(IDC,1).GT.0) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
  140   CONTINUE
      ENDIF
 
      RETURN
      END
