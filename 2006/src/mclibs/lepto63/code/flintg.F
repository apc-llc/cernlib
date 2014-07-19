
C **********************************************************************

      SUBROUTINE FLINTG(CFLQ,CFLG,CFLM)

C...Event-by-event calculation of contribution to longitudinal
C...structure function from QCD and target mass effects.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTEG/ NTOT,NPASS
      EXTERNAL FLQINT,FLGINT,FLTINT

      LQCD=MOD(LST(11),10)
      LTM=MOD(LST(11)/10,10)
      LHT=LST(11)/100
      PARL(25)=ULALPS(Q2)
      IF(LQCD.EQ.2) THEN
C...FL from QCD, quark and gluon contributions.
        ACCUR=PARL(11)
        IT=0
  100   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLQINT,EPS,CFLQ)
        IF(CFLQ.LT.1) THEN
          ACCUR=CFLQ*PARL(11)
          IF(IT.LT.2) GOTO 100
        ENDIF
        ACCUR=PARL(11)
        IT=0
  200   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLGINT,EPS,CFLG)
        IF(CFLG.LT.1.) THEN
          ACCUR=CFLG*PARL(11)
          IF(IT.LT.2) GOTO 200
        ENDIF
      ENDIF
      IF(LTM.EQ.2) THEN
        ACCUR=PARL(11)
        IT=0
  300   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLTINT,EPS,CFLM)
        IF(CFLM.LT.1.) THEN
          ACCUR=CFLM*PARL(11)
          IF(IT.LT.2) GOTO 300
        ENDIF
      ENDIF

      RETURN
      END
