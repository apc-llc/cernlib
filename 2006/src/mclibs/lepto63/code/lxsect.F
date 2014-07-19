
C **********************************************************************

      SUBROUTINE LXSECT

C...Integrate differential cross-section using GADAP, RIWIAD or DIVONNE

      COMMON /LINTEG/ NTOT,NPASS
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      DOUBLE PRECISION ACC,VALUE,ERRIW,FLOW,FHIGH
      COMMON /PARAMS/ ACC,NDIM,NSUB,ITER
      COMMON /ANSWER/ VALUE,ERRIW
      COMMON /BNDLMT/ FLOW,FHIGH
      COMMON /SAMPLE/ NPOINT
      DIMENSION XMINUS(2),XPLUS(2)
      EXTERNAL DCROSS,DLOWER,DUPPER,RIWFUN
      DATA NCALL/0/

      NCALL=NCALL+1
      CALL LTIMEX(TI1)
      NTOT=0
      NPASS=0
      SIGMA=0.
      ERREST=0.
      NDIM=2
C...Parameters for RIWIAD integration.
      ACC=PARL(15)
      NSUB=100
      ITER=100
C...Parameters for DIVON integration.
      DO 20 I=1,2
      XMINUS(I)=0.
   20 XPLUS(I)=1.
      EPS=PARL(15)
      MAXNUM=50000
      FLOW=-1.D0
      FHIGH=1.D+20
      NPOINT=100
C...Additional parameters for detailed DIVON integration.
      SPRDMX=2.
      MAXPTS=50000
      JDEG=0
      NPT=1000

      SIGMA=0.
      ERREST=0.
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1000)
      IF(LST(10).EQ.1) THEN
C...Integration using GADAP.
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1100)
        ACCUR=PARL(15)
        IT=0
  100   IT=IT+1
        ERREST=ACCUR
        CALL GADAP2(XMIN,XMAX,DLOWER,DUPPER,DCROSS,ERREST,SIGMA)
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1110) IT,NTOT,NPASS,SIGMA
        IF(SIGMA.GT.1.) THEN
          IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &    WRITE(6,1120) ACCUR
        ELSE
          IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &    WRITE(6,1130) ACCUR,ACCUR/MAX(1.E-22,SIGMA),PARL(15)
          ACCUR=MAX(1.E-22,SIGMA*PARL(15))
          IF(IT.LT.2) GOTO 100
        ENDIF
      ELSEIF(LST(10).EQ.2) THEN
C...Integration using RIWIAD. When RIWIAD cannot be loaded:
C...activate next two lines and deactivate RIWIAD call.
C       WRITE(6,*) ' RIWIAD not available, execution stopped.'
C       STOP
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1200) SNGL(ACC),NSUB,ITER
        CALL RIWIAD(RIWFUN)
        SIGMA=SNGL(VALUE)
        ERREST=SNGL(ERRIW)
      ELSEIF(LST(10).EQ.3) THEN
C...Integration using simple DIVONNE. When DIVONNE cannot be loaded:
C...activate next two lines and deactivate DIVONNE call.
C       WRITE(6,*) ' DIVONNE not available, execution stopped.'
C       STOP
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1300) EPS,MAXNUM,SNGL(FLOW),SNGL(FHIGH),NPOINT
        CALL DIVON(NDIM,XMINUS,XPLUS,EPS,MAXNUM,SIGMA,ERREST)
      ELSEIF(LST(10).EQ.4) THEN
C...Integration using detailed DIVONNE. When DIVONNE cannot be loaded:
C...activate next two lines and deactivate PARTN and INTGRL calls.
C       WRITE(6,*) ' DIVONNE not available, execution stopped.'
C       STOP
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1400) EPS,MAXNUM,
     &  SNGL(FLOW),SNGL(FHIGH),NPOINT,SPRDMX,MAXPTS,JDEG,NPT
        CALL PARTN(NDIM,XMINUS,XPLUS,SPRDMX,MAXPTS)
        CALL INTGRL(NDIM,JDEG,NPT,SIGMA,ERREST)
      ELSE
        IF(LST(3).GE.1) WRITE(6,*) ' Warning: LST(10) = ',LST(10),
     &  ' not allowed.'
      ENDIF
      CALL LTIMEX(TI2)
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)
     &.OR.(LST(3).GE.1.AND.NPASS.EQ.0)) THEN
        WRITE(6,1500) SIGMA,ERREST,NTOT,NPASS,TI2-TI1
        IF(LST(3).GE.1.AND.NPASS.EQ.0) WRITE(6,1600)
      ENDIF
      PARL(23)=SIGMA

      RETURN
 1000 FORMAT(/,' Integration of cross section:',/,1X,28('-'))
 1100 FORMAT(5X,'using GADAP = adaptive Gaussian integration')
 1110 FORMAT(5X,'Iteration #',I3,/,
     &10X,'# function evaluations; total & non-zero =',2I8,/,
     &10X,'sigma =',G10.2,' pb')
 1120 FORMAT(10X,'required  relative error = ',G10.2)
 1130 FORMAT(10X,'effective absolute error = ',G10.2,/,
     &       10X,'effective relative error = ',G10.2,/,
     &       10X,'required  relative error = ',G10.2)
 1200 FORMAT(5X,'using RIWIAD with parameters: rel. acc. = ',F10.4,
     &/,5X,'# of subvolumes = ',I5,5X,'max # iterations = ',I5)
 1300 FORMAT(5X,'using automatic DIVONNE with parameters: ',
     &'rel. acc. = ',F10.4,/,5X,'max # function calls = ',I5,
     &/,5X,'lower and upper bound on integrand =',2E12.4,
     &/,5X,'# sample points/region =',I5)
 1400 FORMAT(5X,'using detailed DIVONNE with parameters: ',
     &'rel. acc. = ',F10.4,/,5X,'max # function calls = ',I5,
     &/,5X,'lower and upper bound on integrand =',2E12.4,
     &/,5X,'# sample points/region =',I5,
     &/,5X,'SPRDMX, MAXPTS, JDEG, NPT =',F5.2,3I10)
 1500 FORMAT(/,' ===> Cross-section =',1P,G12.3,
     &' pb,  error estimate = ',G12.3,/,
     &6X,'# of integrand evaluations; total & non-zero =',2I8,/,
     &6X,'cpu time for integration =',G12.3,' seconds',/)
 1600 FORMAT(' Warning: integrand always zero, probably no allowed',
     &' phase space due to cuts',/,
     &10X,'check, in particular, CUT(11) to CUT(14)')
      END
