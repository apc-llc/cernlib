CDECK  ID>, HWHPPB.
*CMZ :-        -12/01/93  10.12.43  by  Bryan Webber
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      FUNCTION HWHPPB(S,T,U)
C-----------------------------------------------------------------------
C     Quark box diagram contribution to photon/gluon scattering
C     Internal quark mass neglected: m_q << U,T,S
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWHPPB,S,T,U,S2,T2,U2,PI2,ALNTU,ALNST,ALNSU
      PI2=ACOS(-1.D0)**2
      S2=S**2
      T2=T**2
      U2=U**2
      ALNTU=LOG(T/U)
      ALNST=LOG(-S/T)
      ALNSU=ALNST+ALNTU
      HWHPPB=5.*4.
     & +((2.*S2+2.*(U2-T2)*ALNTU+(T2+U2)*(ALNTU**2+PI2))/S2)**2
     & +((2.*U2+2.*(T2-S2)*ALNST+(T2+S2)* ALNST**2     )/U2)**2
     & +((2.*T2+2.*(U2-S2)*ALNSU+(U2+S2)* ALNSU**2     )/T2)**2
     & +4.*PI2*(((T2-S2+(T2+S2)*ALNST)/U2)**2
     &         +((U2-S2+(U2+S2)*ALNSU)/T2)**2)
      END
