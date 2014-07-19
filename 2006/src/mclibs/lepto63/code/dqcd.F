
C **********************************************************************

      FUNCTION DQCD(ICOSFI,IPART,IP,XP,ZP,Y)

C...First order QCD matrix elements from R.D. Peccei and R. Ruckl:
C...Nucl. Phys. B162 (1980) 125

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,YY,W2,Q2,U
C...Constants C1 to C5 are resp. 2/3/pi, 1/4/pi, 4/3/pi, 1/2/pi, 1/pi
      DATA C1,C2,C3,C4,C5/0.2122066,0.0795775,0.4244132,0.1591549,
     &     0.3183099/

      IF(LST(8).EQ.19.AND.IPART.EQ.1) THEN
C...No QCD Compton for Ariadne
        DQCD=0.0
        RETURN
      ENDIF

      IF(ICOSFI.EQ.0) THEN
        IF(IPART.EQ.1) THEN
          IF(IP.EQ.1) THEN
            DQCD=C1*((ZP**2+XP**2)/(1.-XP)/(1.-ZP)+2.*(XP*ZP+1.))
          ELSEIF(IP.EQ.2) THEN
            DQCD=C1*4.*XP*ZP
          ELSEIF(IP.EQ.3) THEN
            DQCD=C1*((ZP**2+XP**2)/(1.-XP)/(1.-ZP)+2.*(XP+ZP))
          ELSE
            WRITE(6,1000) ICOSFI,IPART,IP
          ENDIF
        ELSEIF(IPART.EQ.2) THEN
          IF(IP.EQ.1) THEN
            DQCD=C2*(XP**2+(1.-XP)**2)*(ZP**2+(1.-ZP)**2)/(1.-ZP)/ZP
          ELSEIF(IP.EQ.2) THEN
            DQCD=C2*8.*XP*(1.-XP)
          ELSEIF(IP.EQ.3) THEN
            DQCD=C2*(XP**2+(1.-XP)**2)*(ZP-(1.-ZP))/(1.-ZP)/ZP
          ELSE
            WRITE(6,1000) ICOSFI,IPART,IP
          ENDIF
        ELSE
          WRITE(6,1000) ICOSFI,IPART,IP
        ENDIF

      ELSEIF(ICOSFI.EQ.1) THEN
        IF(IPART.EQ.1) THEN
          IF(IP.EQ.1) THEN
            DQCD=C3*Y*SQRT((1.-Y)*XP*ZP/(1.-XP)/(1.-ZP))*
     &      (1.-2./Y)*(1.-ZP-XP+2.*XP*ZP)
          ELSEIF(IP.EQ.3) THEN
            DQCD=C3*Y*SQRT((1.-Y)*XP*ZP/(1.-XP)/(1.-ZP))*
     &      (1.-XP-ZP)
          ELSE
            WRITE(6,1000) ICOSFI,IPART,IP
          ENDIF
        ELSEIF(IPART.EQ.2) THEN
          IF(IP.EQ.1) THEN
            DQCD=C4*Y*SQRT((1.-Y)*XP*(1.-XP)/ZP/(1.-ZP))*
     &      (1.-2./Y)*(1.-2.*ZP)*(1.-2.*XP)
          ELSEIF(IP.EQ.3) THEN
            DQCD=C4*Y*SQRT((1.-Y)*XP*(1.-XP)/ZP/(1.-ZP))*
     &      (1.-2.*XP)
          ELSE
            WRITE(6,1000) ICOSFI,IPART,IP
          ENDIF
        ENDIF

      ELSEIF(ICOSFI.EQ.2) THEN
        IF(IPART.EQ.1) THEN
          DQCD=C3*(1.-Y)*XP*ZP
        ELSEIF(IPART.EQ.2) THEN
          DQCD=C5*(1.-Y)*XP*(1.-XP)
        ELSE
          WRITE(6,1000) ICOSFI,IPART,IP
        ENDIF

      ELSE
        WRITE(6,1000) ICOSFI,IPART,IP
      ENDIF
      RETURN

 1000 FORMAT(' Error in routine DQCD     ',
     &' ICOSFI, IPART, IP = ',3I10)
      END
