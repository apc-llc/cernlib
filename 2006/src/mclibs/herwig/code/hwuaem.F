CDECK  ID>, HWUAEM.
*CMZ :-        -23/08/94  13.22.29  by  Mike Seymour
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      FUNCTION HWUAEM(Q2)
C-----------------------------------------------------------------------
C     Running electromagnetic coupling constant.
C     See R. Kleiss et al.: CERN yellow report 89-08, vol.3 p.129
C     Hadronic component from: H. Burkhardt et al.: Z. Phys C43 (89) 497
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWUAEM,HWUAER,Q2,EPS,A1,B1,C1,A2,B2,C2,A3,B3,C3,
     & A4,B4,C4,AEMPI,EEL2,EMU2,ETAU2,ETOP2,REPIGG,X
      LOGICAL FIRST
      EXTERNAL HWUAER
      SAVE FIRST,AEMPI,EEL2,EMU2,ETAU2,ETOP2
      PARAMETER (EPS=1.D-6)
      DATA A1,B1,C1/0.0    ,0.00835,1.000/
      DATA A2,B2,C2/0.0    ,0.00238,3.927/
      DATA A3,B3,C3/0.00165,0.00299,1.000/
      DATA A4,B4,C4/0.00221,0.00293,1.000/
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
         AEMPI=ALPHEM/(3.*PIFAC)
         EEL2 =RMASS(121)**2
         EMU2 =RMASS(123)**2
         ETAU2=RMASS(125)**2
         ETOP2=RMASS(6)**2
         FIRST=.FALSE.
      ENDIF
      IF (ABS(Q2).LT.EPS) THEN
          HWUAEM=ALPHEM
          RETURN
      ENDIF
C Leptonic component
      REPIGG=AEMPI*(HWUAER(EEL2/Q2)+HWUAER(EMU2/Q2)+HWUAER(ETAU2/Q2))
C Hadronic component from light quarks
      X=ABS(Q2)
      IF (X.LT.9.D-2) THEN
          REPIGG=REPIGG+A1+B1*LOG(ONE+C1*X)
      ELSEIF (X.LT.9.D0) THEN
          REPIGG=REPIGG+A2+B2*LOG(ONE+C2*X)
      ELSEIF (X.LT.1.D4) THEN
          REPIGG=REPIGG+A3+B3*LOG(ONE+C3*X)
      ELSE
          REPIGG=REPIGG+A4+B4*LOG(ONE+C4*X)
      ENDIF
C Top Contribution
      REPIGG=REPIGG+AEMPI*HWUAER(ETOP2/Q2)
      HWUAEM=ALPHEM/(ONE-REPIGG)
      RETURN
      END
