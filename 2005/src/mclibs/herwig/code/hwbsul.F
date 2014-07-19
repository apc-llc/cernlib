CDECK  ID>, HWBSUL.
*CMZ :-        -13/07/92  20.15.54  by  Mike Seymour
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWBSUL(Z)
C-----------------------------------------------------------------------
C     LOGARITHMIC PART OF INTEGRAND IN EXPONENT OF SUDAKOV FORM FACTOR.
C     THE SECOND ORDER ALPHAS CASE COMES FROM CONVERTING INTEGRAL OVER
C     Q^2 INTO ONE OVER ALPHAS, WITH FLAVOUR THRESHOLDS.
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWBSUL,HWUALF,Z,QRAT,QLAM,U,AL,BL,QNOW,QMIN,
     & BET(6),BEP(6),MUMI(6),MUMA(6),ALMI(6),ALMA(6),FINT(6),ALFINT,
     & MUMIN,MUMAX,ALMIN,ALMAX
      INTEGER NF
      LOGICAL FIRST
      EXTERNAL HWUALF
      SAVE FIRST,BET,BEP,MUMI,MUMA
      COMMON/HWSINT/QRAT,QLAM
      DATA FIRST/.TRUE./
      ALFINT(AL,BL)=1/BET(NF)*
     &        LOG(BL/(AL*(1+BEP(NF)*BL))*(1+BEP(NF)*AL))
      HWBSUL=0
      U=1.-Z
      IF (SUDORD.EQ.1) THEN
        AL=LOG(QRAT*Z)
        BL=LOG(QLAM*U*Z)
        HWBSUL=LOG(1.-AL/BL)
      ELSE
        IF (FIRST) THEN
          DO 10 NF=3,6
            BET(NF)=(11*CAFAC-2*NF)/(12*PIFAC)
            BEP(NF)=(17*CAFAC**2-(5*CAFAC+3*CFFAC)*NF)/(24*PIFAC**2)
     &              /BET(NF)
            IF (NF.EQ.3) THEN
              MUMI(3)=0
              ALMI(3)=1D30
            ELSE
              MUMI(NF)=RMASS(NF)
              ALMI(NF)=HWUALF(1,MUMI(NF))
            ENDIF
            IF (NF.EQ.6) THEN
              MUMA(NF)=1D30
              ALMA(NF)=0
            ELSE
              MUMA(NF)=RMASS(NF+1)
              ALMA(NF)=HWUALF(1,MUMA(NF))
            ENDIF
            IF (NF.NE.3.AND.NF.NE.6) FINT(NF)=ALFINT(ALMI(NF),ALMA(NF))
 10       CONTINUE
          FIRST=.FALSE.
        ENDIF
        QNOW=QLAM*QCDL3
        QMIN=QNOW/QRAT
        MUMIN=  U*QMIN
        MUMAX=Z*U*QNOW
        IF (MUMAX.LE.MUMIN) RETURN
        ALMIN=HWUALF(1,MUMIN)
        ALMAX=HWUALF(1,MUMAX)
        NF=3
 20     IF (MUMIN.GT.MUMA(NF)) THEN
          NF=NF+1
          GOTO 20
        ENDIF
        IF (MUMAX.LT.MUMA(NF)) THEN
          HWBSUL=ALFINT(ALMIN,ALMAX)
        ELSE
          HWBSUL=ALFINT(ALMIN,ALMA(NF))
          NF=NF+1
 30       IF (MUMAX.GT.MUMA(NF)) THEN
            HWBSUL=HWBSUL+FINT(NF)
            NF=NF+1
            GOTO 30
          ENDIF
          HWBSUL=HWBSUL+ALFINT(ALMI(NF),ALMAX)
        ENDIF
        HWBSUL=HWBSUL*BET(5)
      ENDIF
      END
