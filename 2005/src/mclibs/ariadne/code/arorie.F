C***********************************************************************
C $Id: arorie.F,v 1.2 1996/04/10 12:33:26 mclareni Exp $

      SUBROUTINE ARORIE(I1,I2,I3,BS,B1,B3,QR1,QR3,PT21,PT23)

C...ARiadne subroutine ORIEnt

C...Orients three partons according to recoil strategy determined
C...by QR1 and QR3

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "ludat1.f"

      INXT(I)=IP3(IDO(I))
      IPRV(I)=IP1(IDI(I))


C...Set parton energies and momentum and total energy
      BW=SQRT(BS)
      IF (B1.LE.0.0) CALL ARERRM('ARORIE',9,0)
      DE1=0.5*B1*BW
      IF (B3.LE.0.0) CALL ARERRM('ARORIE',9,0)
      DE3=0.5*B3*BW
      DE2=BW-DE1-DE3
      IF (DE2.LT.BP(I2,5)) CALL ARERRM('ARORIE',9,0)
      DP1=SQRT(DE1**2-BP(I1,5)**2)
      DP2=SQRT(DE2**2-BP(I2,5)**2)
      DP3=SQRT(DE3**2-BP(I3,5)**2)

C...If both partons 1 and 3 can take full recoil choose one according to
C...Kleiss
      IF (QR1.AND.QR3) THEN
        IF (B1**2.GT.(B1**2+B3**2)*RLU(IDUM)) THEN
          QR1=.FALSE.
        ELSE
          QR3=.FALSE.
        ENDIF
      ENDIF

C...Calculate angle between partons 1 and 3
      BCALP=1.0
      IF (DP1.GT.0.0.AND.DP3.GT.0.0) THEN
        BCALP=(DP2**2-DP1**2-DP3**2)/(2.0*DP1*DP3)
      ELSE
        CALL ARERRM('ARORIE',9,0)
      ENDIF
      IF (ABS(BCALP).GT.1.0) CALL ARERRM('ARORIE',9,0)
      BCALP=MAX(-1.0D0,MIN(1.0D0,DBLE(BCALP)))
      BALP=ACOS(BCALP)

C...Determine angle between parton 1 and z-axis
      IF (QR1.AND.PT21.LE.0.0.AND.PT23.LE.0.0) THEN
        BPSI=PARU(1)-BALP
      ELSEIF (QR3.AND.PT21.LE.0.0.AND.PT23.LE.0.0) THEN
        BPSI=0.0
      ELSE
        BPSI=(PARU(1)-BALP)*(B3**2)/(B1**2+B3**2)

C...New recoil strategy
        IF (PT21.GT.0.0.AND.PT21.GE.PT23) THEN
          I0=IPRV(I1)
          BPSI=ARECOI(BP(I0,4),DE1,DE2,DE3,ABS(BP(I0,3)),DP1,DP2,DP3,
     $         BALP,PT21)
        ELSEIF (PT23.GT.0.0.AND.PT23.GT.PT21) THEN
          I4=INXT(I3)
          BPSI=PARU(1)-BALP-
     $         ARECOI(BP(I4,4),DE3,DE2,DE1,ABS(BP(I4,3)),
     $         DP3,DP2,DP1,BALP,PT23)
        ENDIF
      ENDIF

C...Set random azimuth angle
      BGAM=PARU(2)*RLU(IDUM)
      BSGAM=SIN(BGAM)
      BCGAM=COS(BGAM)
      BSPSI=SIN(BPSI)
      BCPSI=COS(BPSI)
      BSPSA=SIN(BPSI+BALP)
      BCPSA=COS(BPSI+BALP)

C...Set fourmomentum of partons
      BP(I1,1)=DP1*BSPSI*BSGAM
      BP(I1,2)=-DP1*BSPSI*BCGAM
      BP(I1,3)=DP1*BCPSI
      BP(I1,4)=DE1

      BP(I3,1)=DP3*BSPSA*BSGAM
      BP(I3,2)=-DP3*BSPSA*BCGAM
      BP(I3,3)=DP3*BCPSA
      BP(I3,4)=DE3

      DZ2=-DP1*BCPSI-DP3*BCPSA
      DT2=DSQRT(MAX(DP2**2-DZ2**2,0.0D0))
      BP(I2,1)=-DT2*BSGAM
      BP(I2,2)=DT2*BCGAM
      BP(I2,3)=DZ2
      BP(I2,4)=DE2

      RETURN

C**** END OF ARORIE ****************************************************
      END
C***********************************************************************
C $Id: arorie.F,v 1.2 1996/04/10 12:33:26 mclareni Exp $

      REAL FUNCTION ARECOI(BE0,DE1,DE2,DE3,BP0,DP1,DP2,DP3,BALP,PT12)

C...Ariadne function RECOIl

C...Calculates the angle of a recoil gluon according to the new
C...Recoil strategy: p_t1^2*exp(-y_1)=p_t2^2*exp(-y_2)

#include "arimpl.f"
#include "ludat1.f"

C...Calculate the maximum and minimum angle
      PHIL=0.0
      PHIU=PARU(1)-BALP

C...Calculate angle of recoil gluon
      BW=DE1+DE2+DE3
      BS=BW**2
      BM3=DE3**2-DP3**2
      S0123=(BW+BE0)**2-BP0**2
      S12=BS-2.0*BW*DE3+BM3
      S23=BS-2.0*BW*DE1
      S13=BS-2.0*BW*DE2
      D01=2.0*S12*DE1*BE0
      D02=2.0*S12*DP1*BP0
      D03=PT12*(S0123-S13-S23+BM3-2.0*DE3*BE0)
      D04=2*DP3*BP0*PT12*COS(BALP)
      D05=2*DP3*BP0*PT12*SIN(BALP)
      D11=D01-D03
      D12=D05
      D13=D04+D02
      D21=(D11**2-D13**2)/(D12**2+D13**2)
      D22=D12*D11/(D12**2+D13**2)
      D31=D22**2-D21
      DSPHI=SQRT(MAX(D31,0.0D0))-D22
      IF (DSPHI.LT.0.0D0) THEN
        PHI=PHIL
      ELSEIF (DSPHI.GE.1.0D0) THEN
        PHI=PHIU
      ELSE
        PHI=MIN(ASIN(DSPHI),DBLE(PARU(1))-BALP)
      ENDIF

      ARECOI=PHI

      RETURN


C**** END OF ARECOI ****************************************************
      END
