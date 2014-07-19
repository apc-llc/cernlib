CDECK  ID>, HWUCFF.
*CMZ :-        -23/08/94  13.22.29  by  Mike Seymour
*-- Author :    Bryan Webber and Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWUCFF(I,J,QSQ,CLF)
C-----------------------------------------------------------------------
C     Calculates basic coefficients in cross-section formula for
C     ffbar --> f'fbar', at virtuality QSQ, I labels initial, J
C     labels final fermion; type given as:
C        I,J= 1- 6: d,u,s,c,b,t
C           =11-16: e,nu_e,mu,nu_mu,tau,nu_tau
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION QSQ,CLF(7),POL1,POL2,QIF,VI,AI,VF,AF,PG,DQM,PMW,
     & DEN,XRE,XIM,XSQ,VI2,AI2,VF2,AF2,PG2,PG12,DQM2,PMW2,DEN2,XRE2,
     & XIM2,XSQ2,XRE12,XIM12
      INTEGER I,J
C Longitudinal Polarisation factors
      POL1=1.-EPOLN(3)*PPOLN(3)
      POL2=PPOLN(3)-EPOLN(3)
C Standard model couplings
      QIF=QFCH(I)*QFCH(J)
      VI=VFCH(I,1)
      AI=AFCH(I,1)
      VF=VFCH(J,1)
      AF=AFCH(J,1)
      PG=POL1*(VI**2+AI**2)+POL2*2.*VI*AI
C Z propagator factors
      DQM=QSQ-RMASS(200)**2
      PMW=GAMZ*RMASS(200)
      DEN=QSQ/(DQM**2+PMW**2)
      XRE=DEN*DQM
      XIM=DEN*PMW
      XSQ=DEN*QSQ
C Calculate cross-section coefficients
      CLF(1)=POL1*QIF**2+XRE*2.*QIF*(POL1*VI+POL2*AI)*VF
     &      +XSQ*PG*(VF**2+AF**2)
      CLF(2)=CLF(1)-2.*XSQ*PG*AF**2
      CLF(3)=2.*(XRE*QIF*(POL1*AI+POL2*VI)*AF
     &      +XSQ*(POL1*2.*VI*AI+POL2*(VI**2+AI**2))*VF*AF)
      IF (TPOL) THEN
         CLF(4)=QIF**2+XRE*2.*QIF*VI*VF+XSQ*(VI**2-AI**2)*(VF**2+AF**2)
         CLF(5)=CLF(4)-2.*XSQ*(VI**2-AI**2)*AF**2
         CLF(6)=XIM*2.*QIF*AI*VF
         CLF(7)=CLF(6)
      ENDIF
      IF (ZPRIME) THEN
C Z' couplings:
         VI2=VFCH(I,2)
         AI2=AFCH(I,2)
         VF2=VFCH(J,2)
         AF2=AFCH(J,2)
         PG2=POL1*(VI2**2+AI2**2)+POL2*2.*VI2*AI2
         PG12=POL1*(VI*VI2+AI*AI2)+POL2*(VI*AI2+AI+VI2)
C Z' propagator factors
         DQM2=QSQ-RMASS(202)**2
         PMW2=RMASS(202)*GAMZP
         DEN2=QSQ/(DQM2**2+PMW2**2)
         XRE2=DEN2*DQM2
         XIM2=DEN2*PMW2
         XSQ2=DEN2*QSQ
         XRE12=DEN*DEN2*(DQM*DQM2+PMW*PMW2)
         XIM12=DEN*DEN2*(DQM*PMW2-DQM2*PMW)
C Additional contributions to cross-section coefficients
         CLF(1)=CLF(1)+XRE2*2.*QIF*(POL1*VI2+POL2*AI2)*VF2
     &    +XSQ2*PG2*(VF2**2+AF2**2)+XRE12*2.*PG12*(VF*VF2+AF*AF2)
         CLF(2)=CLF(1)-2.*(XSQ2*PG2*AF2**2+XRE12*2.*PG12*AF*AF2)
         CLF(3)=CLF(3)+2.*(XRE2*QIF*(POL1*AI2+POL2*VI2)*AF2
     &    +XSQ2*(POL1*2.*VI2*AI2+POL2*(VI2**2+AI2**2))*VF2*AF2
     &    +XRE12*(POL1*(VI*AI2+AI*VI2)+POL1*(VI*VI2+AI*AI2))
     &    *(VF*VF2+AF*AF2))
         IF (TPOL) THEN
            CLF(4)=CLF(4)+XRE2*2.*QIF*VI2*VF2
     &       +XSQ2*(VI2**2-AI2**2)*(VF2**2+AF2**2)
     &       +XRE12*2.*(VI*VI2-AI*AI2)*(VF*VF2+AF*AF2)
            CLF(5)=CLF(4)-2*(XSQ2*(VI2**2-AI2**2)*AF2**2
     &       +XRE12*2.*(VI*VI2-AI*AI2)*AF*AF2)
            CLF(6)=CLF(6)+2.*(XIM2*QIF*AI2*VF2
     &       -XIM12*(VI*AI2-AI*VI2)*(VF*VF2+AF*AF2))
            CLF(7)=CLF(6)+4.*XIM12*(VI*AI2-AI*AI2)*AF*AF2
         ENDIF
      ENDIF
      RETURN
      END
