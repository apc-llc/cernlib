
C#######################################################################
C
C   The following routines are slightly modified minimization routines
C   from the MINUIT program package.
C
C **********************************************************************

      SUBROUTINE LMCMND

C...This is the MINUIT routine COMAND.
CC        GETS IFORMATION FROM /LMINUI/  AND TAKES APPROPRIATE ACTION,
CC        EITHER DIRECTLY BY SKIPPING TO THE CORRESPONDING CODE IN
CC        LMCMND, OR BY SETTING UP A CALL TO A SUBROUTINE
CC
      COMMON /LMINUI/ XKIN(4),UKIN(4),WKIN(4),AIN(4),BIN(4),
     &MAXFIN,RELUP,RELERR,RELER2,FCNMAX
      COMMON /LPFLAG/ LST3

      COMMON
     1/LMMINE/ ERP(30)  ,ERN(30)
     2/LMPARI/ X(15)    ,XT(15)   ,DIRIN(15) ,MAXINT     ,NPAR
     3/LMPARE/ U(30)              ,WERR(30)  ,MAXEXT     ,NU
     4/LMLIMI/ ALIM(30) ,BLIM(30) ,LCODE(30) ,LCORSP(30) ,LIMSET
     5/LMVARI/ V(15,15)
     7/LMFIX / IPFIX(15),XS(15)   ,XTS(15)   ,DIRINS(15) ,NPFIX
     7/LMFIX2/ GRDS(15) ,G2S(15)  ,GSTEPS(15),ABERFS(15)
     C/LMCASC/ Y(16)    ,JH       ,JL
     F/LMDERI/ GIN(30)  ,GRD(15)  ,G2(15)    ,GSTEP(15)  ,ABERF(15)
     G/LMSIMV/ P(15,16) ,PSTAR(15),PSTST(15) ,PBAR(15)   ,PRHO(15)
     J/LMVART/ VT(15,15)
      COMMON
     6/LMUNIT/ ISYSRD   ,ISYSWR   ,ISYSPU
     8/LMTITL/ TITLE(13),DATE(2)  ,ISW(7)    ,NBLOCK
     9/LMCONV/ EPSI     ,APSI     ,VTEST     ,NSTEPQ     ,NFCN ,NFCNMX
     A/LMCARD/ CWORD    ,CWORD2   ,CWORD3    ,WORD7(7)
     B/LMMINI/ AMIN     ,UP       ,NEWMIN    ,ITAUR      ,SIGMA,EPSMAC
      FVAL3 = 2.0*AMIN+1.0
C                                        . . . . . . . . . . ERROR DEF
      WORD7(1)=RELUP*ABS(AMIN)
      UP = WORD7(1)
      IF (UP .LE. 0.)  UP = 1.0
      IF (ISW(2) .GE. 1)  CALL LMPRIN(1,AMIN)
      WORD7(1)=MAXFIN
      WORD7(2)=RELERR*UP
      NFCNMX = WORD7(1) + 0.5
      IF (NFCNMX .LE. 0)  NFCNMX = 1000
      EPSI = WORD7(2)
      IF (EPSI .LE. 0.)  EPSI = 0.1 * UP
      NEWMIN = 0
      ITAUR = 0
      ISW(1) = 0
      CALL LMSIMP
      IF(ABS(DIRIN(1)).LE.ABS(EPSMAC*X(1)).AND.
     &   ABS(DIRIN(2)).LE.ABS(EPSMAC*X(2))) THEN
        IF(LST3.GE.1) WRITE(6,2100)
        GOTO 500
      ENDIF
      WORD7(1)=MAXFIN
      RELERR=RELER2*RELERR
      WORD7(2)=RELERR*UP
      NFCNMX = WORD7(1) + 0.5
      IF (NFCNMX .LE. 0)  NFCNMX = 1000
      EPSI = WORD7(2)
      IF (EPSI .LE. 0.)  EPSI = 0.1 * UP
      CALL LMSIMP
  500 FCNMAX=ABS(AMIN)
      IF(ISW(1).GE.1) THEN
        IF(LST3.GE.1) WRITE(6,2200)
        FCNMAX=FCNMAX*1.25
      ENDIF
      FMAX=ABS(AMIN)
C                                        . . . . . . . . . . END, EXIT
      WORD7(1)=0.
 1100 IT = WORD7(1) + 0.5
      IF (FVAL3 .EQ. AMIN .OR. IT .GT. 0) RETURN
      IFLAG = 3
      CALL LSIGMX(NPAR,GIN,F,U,IFLAG)
      NFCN = NFCN + 1
      IF(LST3.GE.1.AND.ABS(F).GT.FMAX) WRITE(6,2300) F
      RETURN

 2100 FORMAT(' Warning: Stepsizes are less than machine accuracy ',
     &'times variable values. No further minimization attempted.')
 2200 FORMAT(' Warning: Simplex minimization has not converged ',
     &'properly.',/,10X,'Returned maximum increased by a factor 1.25.')
 2300 FORMAT(' Warning from LMCMND: function at minimum, ',E12.4,
     &'  is smaller than stored minimum.')

      END
