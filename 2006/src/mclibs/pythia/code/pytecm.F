 
C*********************************************************************
 
C...PYTECM
C...Finds the s-hat dependent eigenvalues of the inverse propagator
C...matrix for gamma, Z, techni-rho, and techni-omega to optimize the
C...phase space generation.
 
      SUBROUTINE PYTECM(S1,S2)
 
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
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYDAT1/,/PYDAT2/,/PYPARS/
 
C...Local variables.
      DOUBLE PRECISION AR(4,4),WR(4),ZR(4,4),ZI(4,4),WORK(12,12),
     &AT(4,4),WI(4),FV1(4),FV2(4),FV3(4),sh,aem,tanw,ct2w,qupd,alprht,
     &far,fao,fzr,fzo,shr,R1,R2,S1,S2,WDTP(0:400),WDTE(0:400,0:5)
      INTEGER i,j,ierr
 
      SH=PMAS(PYCOMP(KTECHN+113),1)**2
      AEM=PYALEM(SH)
 
      TANW=SQRT(PARU(102)/(1D0-PARU(102)))
      CT2W=(1D0-2D0*PARU(102))/(2D0*PARU(102)/TANW)
      QUPD=2D0*PARP(143)-1D0
 
      ALPRHT=2.91D0*(3D0/PARP(144))
      FAR=SQRT(AEM/ALPRHT)
      FAO=FAR*QUPD
      FZR=FAR*CT2W
      FZO=-FAO*TANW
 
      AR(1,1) = SH
      AR(2,2) = SH-PMAS(23,1)**2
      AR(3,3) = SH-PMAS(PYCOMP(KTECHN+113),1)**2
      AR(4,4) = SH-PMAS(PYCOMP(KTECHN+223),1)**2
      AR(1,2) = 0D0
      AR(2,1) = 0D0
      AR(1,3) = -SH*FAR
      AR(3,1) = AR(1,3)
      AR(1,4) = -SH*FAO
      AR(4,1) = AR(1,4)
      AR(2,3) = -SH*FZR
      AR(3,2) = AR(2,3)
      AR(2,4) = -SH*FZO
      AR(4,2) = AR(2,4)
      AR(3,4) = 0D0
      AR(4,3) = 0D0
CCCCCCCC
      DO 110 I=1,4
        DO 100 J=1,4
          AT(I,J)=0D0
  100   CONTINUE
  110 CONTINUE
      SHR=SQRT(SH)
      CALL PYWIDT(23,SH,WDTP,WDTE)
      AT(2,2) = WDTP(0)*SHR
      CALL PYWIDT(KTECHN+113,SH,WDTP,WDTE)
      AT(3,3) = WDTP(0)*SHR
      CALL PYWIDT(KTECHN+223,SH,WDTP,WDTE)
      AT(4,4) = WDTP(0)*SHR
CCCC
      CALL PYEICG(4,4,AR,AT,WR,WI,0,ZR,ZI,FV1,FV2,FV3,IERR)
      DO 120 I=1,4
        WI(I)=SQRT(ABS(SH-WR(I)))
        WR(I)=ABS(WR(I))
  120 CONTINUE
      R1=MIN(WR(1),WR(2),WR(3),WR(4))
      R2=1D20
      S1=0D0
      S2=0D0
      DO 130 I=1,4
        IF(ABS(WR(I)-R1).LT.1D-6) THEN
          S1=WI(I)
          GOTO 130
        ENDIF
        IF(WR(I).LE.R2) THEN
          R2=WR(I)
          S2=WI(I)
        ENDIF
  130 CONTINUE
      S1=S1**2
      S2=S2**2
      RETURN
      END
