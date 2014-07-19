
C **********************************************************************

      FUNCTION DQCDI(IPART,IP,XP,ZPMIN,ZPMAX)

C...First order QCD matrix elements as in function DQCD but analytically
C...integrated over ZP from ZPMIN to ZPMAX, also a factor 1/(1-XP) is
C...factored out (since XP chosen randomly according to 1/(1-XP) distr.)

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      DATA C1,C2/0.2122066,0.0795775/

      IF(LST(8).EQ.19.AND.IPART.EQ.1) THEN
C...No QCD Compton for Ariadne
        DQCDI=0.0
        RETURN
      ENDIF

      IF(IPART.EQ.1) THEN
        IF(IP.EQ.1) THEN
          ZLOG=ALOG(ZPMAX/ZPMIN)
          DQCDI=C1*(XP**2*ZLOG+ZPMIN-ZPMAX+(ZPMIN**2-ZPMAX**2)/2.+ZLOG+
     &    XP*(1.-XP)*(ZPMAX**2-ZPMIN**2)+2.*(1.-XP)*(ZPMAX-ZPMIN))
        ELSEIF(IP.EQ.2) THEN
          DQCDI=C1*2.*XP*(1.-XP)*(ZPMAX**2-ZPMIN**2)
        ELSEIF(IP.EQ.3) THEN
          ZLOG=ALOG(ZPMAX/ZPMIN)
          DQCDI=C1*(XP**2*ZLOG+ZPMIN-ZPMAX+(ZPMIN**2-ZPMAX**2)/2.+ZLOG+
     &    2.*XP*(1.-XP)*(ZPMAX-ZPMIN)+(1.-XP)*(ZPMAX**2-ZPMIN**2))
        ELSE
          WRITE(6,1000) IPART,IP
        ENDIF

      ELSEIF(IPART.EQ.2) THEN
        IF(IP.EQ.1) THEN
          DQCDI=C2*(1.-XP)*(XP**2+(1.-XP)**2)*(2.*(ZPMIN-ZPMAX)+
     &    ALOG(ZPMAX**2/ZPMIN**2))
        ELSEIF(IP.EQ.2) THEN
          DQCDI=C2*8.*XP*(1.-XP)**2*(ZPMAX-ZPMIN)
        ELSEIF(IP.EQ.3) THEN
          DQCDI=0.
        ELSE
          WRITE(6,1000) IPART,IP
        ENDIF

      ELSE
        WRITE(6,1000) IPART,IP
      ENDIF
      RETURN

 1000 FORMAT(' Error in routine DQCDI     ',
     &' IPART, IP = ',2I10)
      END
