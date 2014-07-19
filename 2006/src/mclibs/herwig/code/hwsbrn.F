CDECK  ID>, HWSBRN.
*CMZ :-        -15/07/92  14.08.45  by  Mike Seymour
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWSBRN(KPAR)
C-----------------------------------------------------------------------
C     DOES BRANCHING OF SPACELIKE PARTON KPAR
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWBVMC,HWRGEN,HWRUNI,HWSTAB,HWUALF,HWUTAB,HWSGQQ,
     & HWSSUD,XLAST,QNOW,QLST,QP,QMIN,QLAM,QSAV,SMAX,SLST,SNOW,RN,SUDA,
     & SUDB,ZZ,ENOW,XI,PMOM,DIST(13),DMIN,X1,X2,REJFAC,OTHXI,OTHZ,QTMP,
     & PTMP(2)
      INTEGER N0,IS,ID,ID1,ID2,IDHAD,N1,I,MQ,NTRY,NDEL,NA,NB,IW1,IW2,
     & KPAR,LPAR,MPAR,ISUD(13),IREJ,NREJ
      LOGICAL HWSVAL,FORCE,VALPAR,FTMP
      EXTERNAL HWBVMC,HWRGEN,HWRUNI,HWSTAB,HWUALF,HWUTAB,HWSGQQ,HWSSUD,
     & HWSVAL
      COMMON/HWTABC/XLAST,N0,IS,ID
      DATA ISUD,DMIN/2,2,3,4,5,6,2,2,3,4,5,6,1,1.D-15/
      IF (IERROR.NE.0) RETURN
      ID=IDPAR(KPAR)
C--TEST FOR PARTON TYPE
      IF (ID.LE.13) THEN
        IS=ISUD(ID)
      ELSEIF (ID.GE.208) THEN
        IS=7
      ELSE
        IS=0
      END IF
      QNOW=-1.
      IF (IS.NE.0) THEN
C--SPACELIKE PARTON BRANCHING
        QLST=PPAR(1,KPAR)
        IDHAD=IDHW(INHAD)
        VALPAR=HWSVAL(ID)
        QP=HWBVMC(ID)
        XLAST=XFACT*PPAR(4,KPAR)
        IF (XLAST.GE.1.) CALL HWWARN('HWSBRN',107,*999)
C--SET UP Q BOUNDARY
        IF (VALPAR) THEN
          QMIN=QG/(1.-XLAST)
        ELSEIF (ID.EQ.13) THEN
          QMIN=QV/(1.-XLAST)
        ELSE
          QMIN=.5*(QP+QV+SQRT((QP-QV)**2+4.*QP*QV*XLAST))/(1.-XLAST)
        ENDIF
        QSAV=QMIN
        IF (QMIN.LE.QSPAC.AND.ISPAC.LT.2) THEN
          QMIN=QSPAC
          N1=NSPAC(IS)
        ELSEIF (QMIN.LE.QEV(1,IS)) THEN
          QMIN=QEV(1,IS)
          N1=1
        ELSE
          DO 110 I=2,NQEV
          IF (QEV(I,IS).GT.QMIN) GOTO 120
  110     CONTINUE
  120     N1=I-1
        ENDIF
        N0=N1-1
        MQ=NQEV-N0
        NTRY=0
  125   NTRY=NTRY+1
        NREJ=1
        IF (QLST.GT.QMIN.AND..NOT.NOSPAC.OR..NOT.VALPAR) THEN
          IF (QLST.LE.QMIN) THEN
C--CHECK PHASE SPACE FOR FORCED SPLITTING OF NON-VALENCE PARTON
            IF (QLST.LT.QSAV) CALL HWWARN('HWSBRN',ISLENT*105,*999)
            FORCE=.TRUE.
            QNOW=(QLST/QSAV)**HWRGEN(0)*QSAV
          ELSE
C--ENHANCE EMISSION BY A FACTOR OF TWO IF THIS BRANCH
C  IS CAPABLE OF BEING THE HARDEST SO FAR
           IF (QLST.GT.HARDST) NREJ=2
           QTMP=-1
           DO 300 IREJ=1,NREJ
C--FIND NEW VALUE OF SUD/DIST
            CALL HWSFUN(XLAST,QMIN,IDHAD,NSTRU,DIST,JNHAD)
            IF (ID.EQ.13) DIST(ID)=DIST(ID)*HWSGQQ(QMIN)
            IF (DIST(ID).LT.DMIN) DIST(ID)=DMIN
            SMAX=HWUTAB(SUD(N1,IS),QEV(N1,IS),MQ,QMIN,INTER)/DIST(ID)
            CALL HWSFUN(XLAST,QLST,IDHAD,NSTRU,DIST,JNHAD)
            IF (ID.EQ.13) DIST(ID)=DIST(ID)*HWSGQQ(QLST)
            IF (DIST(ID).LT.DMIN) DIST(ID)=DMIN
            SLST=HWUTAB(SUD(N1,IS),QEV(N1,IS),MQ,QLST,INTER)/DIST(ID)
            RN=HWRGEN(0)
            IF (RN.EQ.0.) THEN
              SNOW=SLST*2.
            ELSE
              SNOW=SLST/RN
            ENDIF
            IF (VALPAR.AND.SNOW.GE.SMAX) GOTO 200
            IF (SNOW.LT.SMAX.AND..NOT.NOSPAC) THEN
              FORCE=.FALSE.
            ELSE
C--FORCE SPLITTING OF NON-VALENCE PARTON
              FORCE=.TRUE.
              QNOW=(MIN(QLST,1.1*QMIN)/QSAV)**HWRGEN(0)*QSAV
            ENDIF
            IF (QNOW.LT.0.) THEN
C--BRANCHING OCCURS. FIRST CHECK FOR MONOTONIC FORM FACTOR
              SUDA=SMAX
              NDEL=32
              NA=N1
  130         NB=NA+NDEL
              IF (NB.GT.NQEV) CALL HWWARN('HWSBRN',103,*999)
              CALL HWSFUN(XLAST,QEV(NB,IS),IDHAD,NSTRU,DIST,JNHAD)
              IF (ID.EQ.13) DIST(ID)=DIST(ID)*HWSGQQ(QEV(NB,IS))
              IF (DIST(ID).LT.DMIN) DIST(ID)=DMIN
              SUDB=SUD(NB,IS)/DIST(ID)
              IF (SUDB.GT.SUDA) THEN
                SUDA=SUDB
                NA=NB
                GOTO 130
              ELSEIF (NA.NE.N1) THEN
                IF (SUDB.LT.SNOW) THEN
                  NDEL=NDEL/2
                  IF (NDEL.EQ.0) CALL HWWARN('HWSBRN',100,*999)
                  GOTO 130
                ENDIF
                N1=NB
                N0=N1-1
                MQ=NQEV-N0
              ENDIF
C--NOW FIND NEW Q
              QNOW=HWSTAB(QEV(N1,IS),HWSSUD,MQ,SNOW,INTER)
              IF (QNOW.LE.QMIN.OR.QNOW.GT.QLST) THEN
C--INTERPOLATION PROBLEM: USE LINEAR INSTEAD
C                CALL HWWARN('HWSBRN',1,*999)
                QNOW=HWRUNI(0,QMIN,QLST)
              ENDIF
            ENDIF
 200        CONTINUE
            IF (QNOW.GT.QTMP) THEN
              QTMP=QNOW
              FTMP=FORCE
            ENDIF
            QNOW=-1
 300       CONTINUE
           QNOW=QTMP
           FORCE=FTMP
          ENDIF
          IF (QNOW.LT.0) GOTO 210
C--NOW FIND NEW X
          CALL HWSFBR(XLAST,QNOW,FORCE,ID,1,ID1,ID2,IW1,IW2,ZZ)
          IF (ID1.LT.0) THEN
C--NO PHASE SPACE FOR BRANCHING
            CALL HWWARN('HWSBRN',ISLENT*101,*999)
          ELSEIF (ID1.EQ.0) THEN
C--BRANCHING REJECTED: REDUCE Q AND REPEAT
            IF (NTRY.GT.NBTRY.OR.IERROR.NE.0)
     $           CALL HWWARN('HWSBRN',102,*999)
            QLST=QNOW
            QNOW=-1.
            GOTO 125
          ELSEIF (ID1.EQ.59) THEN
C--ANOMALOUS PHOTON SPLITTING: ADD PT TO INTRINSIC PT AND STOP BRANCHING
            IF (IDHAD.NE.59) CALL HWWARN('HWSBRN',109,*999)
            ENOW=PPAR(4,KPAR)/XLAST
            XI=(QNOW/ENOW)**2
            QLAM=QNOW*(1.-XLAST)
            IF ((2.-XI)*QLAM**2.GT.EMSCA**2) THEN
C--BRANCHING REJECTED: REDUCE Q AND REPEAT
              IF (NTRY.GT.NBTRY) CALL HWWARN('HWSBRN',110,*999)
              QLST=QNOW
              QNOW=-1.
              GOTO 125
            ENDIF
            CALL HWRAZM(QNOW*(1.-XLAST),PTMP(1),PTMP(2))
            CALL HWVSUM(2,PTMP,PTINT(1,JNHAD),PTINT(1,JNHAD))
            PTINT(3,JNHAD)=PTINT(1,JNHAD)**2+PTINT(2,JNHAD)**2
            ANOMSC(1,JNHAD)=QNOW
            ANOMSC(2,JNHAD)=QNOW*(1.-XLAST)
            QNOW=-1.
            QLST=QNOW
            GOTO 125
          ELSEIF (FORCE.AND..NOT.HWSVAL(ID1).AND.ID1.NE.13) THEN
C--FORCED BRANCHING PRODUCED A NON-VALENCE PARTON: TRY AGAIN
            IF (NTRY.GT.NBTRY) CALL HWWARN('HWSBRN',108,*999)
            QLST=QNOW
            QNOW=-1.
            GOTO 125
          ENDIF
        ENDIF
  210   CONTINUE
        IF (QNOW.GT.0.) THEN
C--BRANCHING HAS OCCURRED
          ENOW=PPAR(4,KPAR)/ZZ
          XI=(QNOW/ENOW)**2
          QLAM=QNOW*(1.-ZZ)
          IF (SUDORD.EQ.1.AND.HWUALF(2,QLAM).LT.HWRGEN(0) .OR.
     &        (2.-XI)*QLAM**2.GT.EMSCA**2.AND..NOT.FORCE) THEN
C--BRANCHING REJECTED: REDUCE Q AND REPEAT
              IF (NTRY.GT.NBTRY) CALL HWWARN('HWSBRN',104,*999)
              QLST=QNOW
              QNOW=-1.
              GOTO 125
          ENDIF
C--IF THIS IS HARDEST EMISSION SO FAR, APPLY MATRIX-ELEMENT CORRECTION
          IF (.NOT.FORCE) THEN
            REJFAC=1
            IF (QLAM.GT.HARDST) THEN
              IF (MOD(ISTHEP(JCOPAR(1,1)),10).GE.3) THEN
C---COLOUR PARTNER IS OUTGOING (X1=XP, X2=ZP)
                X2=SQRT((ZZ**2-(1-ZZ)*XI)**2+2*(ZZ*(1-ZZ))**2*XI*(2-XI))
                X1=(ZZ**2+(1-ZZ)*XI-X2)/(2*(1-ZZ)*XI)
                X2=(ZZ**2-(1-ZZ)*XI+X2)/(2*ZZ**2)
                IF (ID2.EQ.13) THEN
C---GLUON EMISSION
                  REJFAC=ZZ**3*(1-X1-X2+2*X1*X2)
     $                 /(X1**2*(1-ZZ)*(ZZ+XI*(1-ZZ)))
     $                 *(1+ZZ**2)/((1-ZZ)*XI)
     $                 *(1-X1)*(1-X2)/
     $                 (1+(1-X1-X2+2*X1*X2)**2+2*(1-X1)*(1-X2)*X1*X2)
C---CHECK WHETHER IT IS IN THE OVERLAP REGION
                  OTHXI=2*(1-X1)/(1-X1+2*(3*X1-2)*X2*(1-X2))
                  IF (OTHXI.LT.1) THEN
                    OTHZ=(1-(2*X2-1)*SQRT((3*X1-2)/X1))/2
                    REJFAC=REJFAC+SQRT(3-2/X1)/(X1**2*OTHZ*(1-OTHZ))
     $               *(1+(1-OTHZ)**2)/(OTHZ*OTHXI)
     $               *(1-X1)*(1-X2)/
     $               (1+(1-X1-X2+2*X1*X2)**2+2*(1-X1)*(1-X2)*X1*X2)
                  ENDIF
                ELSEIF (ID1.EQ.13) THEN
C---GLUON SPLITTING
                  REJFAC=ZZ**3*(1-X1-X2+2*X1*X2)
     $                 /(X1**2*(1-ZZ)*(ZZ+XI*(1-ZZ)))
     $                 *(ZZ**2+(1-ZZ)**2)/XI
     $                 *(1-X2)/
     $                 ((  X1+X2-2*X1*X2)**2+2*(1-X1)*(1-X2)*X1*X2
     $                 +(1-X1-X2+2*X1*X2)**2+2*(1-X1)*(1-X2)*X1*X2)
                ENDIF
              ELSE
C---COLOUR PARTNER IS ALSO INCOMING (NOT IMPLEMENTED YET)
                REJFAC=1
              ENDIF
            ENDIF
            IF (NREJ*REJFAC*HWRGEN(NREJ).GT.1) THEN
              QLST=QNOW
              QNOW=-1.
              GOTO 125
            ENDIF
            IF (QLAM.GT.HARDST) HARDST=QLAM
          ENDIF
          IF (IW2.GT.IW1) THEN
            LPAR=NPAR+1
            MPAR=NPAR+2
C---NEW MOTHER-DAUGHTER RELATIONS
C   N.B. DEFINED MOVING AWAY FROM HARD PROCESS
            JDAPAR(1,KPAR)=LPAR
            JDAPAR(2,KPAR)=MPAR
C---NEW COLOUR CONNECTIONS
            JCOPAR(3,KPAR)=MPAR
            JCOPAR(4,KPAR)=LPAR
            JCOPAR(1,MPAR)=KPAR
            JCOPAR(2,MPAR)=LPAR
            JCOPAR(1,LPAR)=MPAR
            JCOPAR(2,LPAR)=KPAR
          ELSE
            MPAR=NPAR+1
            LPAR=NPAR+2
            JDAPAR(1,KPAR)=MPAR
            JDAPAR(2,KPAR)=LPAR
            JCOPAR(3,KPAR)=LPAR
            JCOPAR(4,KPAR)=MPAR
            JCOPAR(1,MPAR)=LPAR
            JCOPAR(2,MPAR)=KPAR
            JCOPAR(1,LPAR)=KPAR
            JCOPAR(2,LPAR)=MPAR
          ENDIF
          JMOPAR(1,LPAR)=KPAR
          JMOPAR(1,MPAR)=KPAR
          IDPAR(LPAR)=ID1
          IDPAR(MPAR)=ID2
          TMPAR(LPAR)=.FALSE.
          TMPAR(MPAR)=.TRUE.
          PPAR(1,LPAR)=QNOW
          PPAR(2,LPAR)=XI
          PPAR(4,LPAR)=ENOW
          PPAR(1,MPAR)=QNOW*(1.-ZZ)
          PPAR(2,MPAR)=XI
          PPAR(4,MPAR)=ENOW*(1.-ZZ)
          NPAR=NPAR+2
        ENDIF
      ENDIF
      IF (QNOW.LT.0.) THEN
C--BRANCHING STOPS
        JDAPAR(1,KPAR)=0
        JDAPAR(2,KPAR)=0
        JCOPAR(3,KPAR)=0
        JCOPAR(4,KPAR)=0
        IF (ID.LE.13) THEN
          IF (IDHEP(INHAD).EQ.22) THEN
C---PUT SPECTATOR QUARK IN PHOTON ON-SHELL
            XLAST=XFACT*PPAR(4,KPAR)
            IF ((1-XLAST)**2.LT.(RMASS(ID)**2+PTINT(3,JNHAD))*XFACT**2)
     &           THEN
              FROST=.TRUE.
              RETURN
            ENDIF
            PPAR(5,KPAR)=-(RMASS(ID)**2*XLAST+PTINT(3,JNHAD))/(1.-XLAST)
     &           +XLAST*SIGN(PHEP(5,INHAD)**2,PHEP(5,INHAD))
          ELSE
            PPAR(5,KPAR)=-RMASS(ID)**2-PTINT(3,JNHAD)
            IF (PPAR(5,KPAR).LT.-QSPAC**2) PPAR(5,KPAR)=-QSPAC**2
          ENDIF
        ELSEIF (ID.EQ.IDHW(INHAD)) THEN
C---IF INCOMING PARTON IS INCOMING BEAM, ALLOW IT TO BE OFF-SHELL
          PPAR(5,KPAR)=SIGN(PHEP(5,INHAD)**2,PHEP(5,INHAD))
        ELSE
          PPAR(5,KPAR)=RMASS(ID)**2
        ENDIF
        PMOM=PPAR(4,KPAR)**2-PPAR(5,KPAR)
        IF (PMOM.LT.0.) THEN
          FROST=.TRUE.
          RETURN
        ENDIF
        PPAR(3,KPAR)=SQRT(PMOM)
      ENDIF
  999 END
