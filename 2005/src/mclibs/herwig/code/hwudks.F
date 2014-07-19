CDECK  ID>, HWUDKS.
*CMZ :-        -
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWUDKS
C-----------------------------------------------------------------------
C     Sets up internal pointers based on the decay table in HWUDAT or as
C     supplied via HWIODK. Computes CoM momenta of two-body decay modes.
C     Particles with long lifetimes or no allowed decay (excepting light
C     b hadrons when CLEO/EURODEC decays requested) are set stable, else
C     calculate DKLTM(I) = mass/width ( = mass * lifetime/hbar).
C     Gives warnings if: a particle has no decay modes or antiparticle's
C     modes are not the charge conjugates of the particles.
C     (N.B. CP violation permits this).
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWUPCM,HWUAEM,BRSUM,EPS,SCALE,BRTMP(NMXMOD),FN,X,
     & W,Q,FAC
      INTEGER HWUANT,I,IDKY,LAST,LTMP(NMXMOD),J,L,K,M,N,INDX(NMXMOD),
     & IRES,IAPDG,IPART,LR,LP,KPRDLR
      LOGICAL BPDK,TOPDKS,MATCH(5),PMATCH(NMXMOD)
      CHARACTER*7 CVETO(2)
      CHARACTER*8 CDUM
      EXTERNAL HWUPCM,HWUAEM,HWUANT
      PARAMETER(EPS=1.E-6)
      FN(X,Q,W)=X**6/(((X*X-Q*Q)**2+W*W*(X*X+Q*Q)-2.*W**4)
     &               *SQRT(X**4+Q**4+W**4-2.*(X*X*Q*Q+X*X*W*W+Q*Q*W*W)))
      WRITE(6,10)
  10  FORMAT(/10X,'Checking consistency of decay tables'/)
      DKPSET=.TRUE.
C First zero arrays
      DO 20 I=1,NMXRES
      LSTRT(I)=0
  20  NMODES(I)=0
      DO 30 I=1,NMXDKS
      NPRODS(I)=0
      LNEXT(I)=0.
  30  CMMOM(I)=0
      BPDK=BDECAY.NE.'HERW'
      DO 180 I=1,NDKYS
C Search for next decaying particle type
      IDKY=IDK(I)
C Skip if particle is not recognised or already dealt with
      IF (IDKY.EQ.0.OR.IDKY.EQ.20) THEN
        WRITE(6,40) I
  40    FORMAT(1X,'Line ',I4,': decaying particle not recognised')
        GOTO 180
      ENDIF
      IF (NMODES(IDKY).GT.0) GOTO 180
C Check and include first decay mode, storing a copy
      CALL HWDCHK(IDKY,I,*180)
      LSTRT(IDKY)=I
      NMODES(IDKY)=1
      BRSUM=BRFRAC(I)
      LTMP(1)=I
      BRTMP(1)=-BRFRAC(I)
      LAST=I
C Sets CMMOM(IDKY) = CoM momentum for first 2-body decay mode I (else 0)
      IF (NPRODS(I).EQ.2) CMMOM(I)=
     & HWUPCM(RMASS(IDKY),RMASS(IDKPRD(1,I)),RMASS(IDKPRD(2,I)))
C Include any other decay modes of IDKY
      DO 120 J=I+1,NDKYS
      IF (IDK(J).EQ.IDKY) THEN
C First see if it is a copy of the same decay channel
        IF ((IDKPRD(2,J).GE.1.AND.IDKPRD(2,J).LE.13).OR.
     &      (IDKPRD(3,J).GE.1.AND.IDKPRD(3,J).LE.13)) THEN
C Partonic respect order
          L=LSTRT(IDKY)
          DO 50 K=1,NMODES(IDKY)
              IF (IDKPRD(1,L).EQ.IDKPRD(1,J).AND.
     &            IDKPRD(2,L).EQ.IDKPRD(2,J).AND.
     &            IDKPRD(3,L).EQ.IDKPRD(3,J).AND.
     &            IDKPRD(4,L).EQ.IDKPRD(4,J).AND.
     &            IDKPRD(5,L).EQ.IDKPRD(5,J)) GOTO 100
  50      L=LNEXT(L)
        ELSE
C Allow for different order in matching
          L=LSTRT(IDKY)
          DO 90 K=1,NMODES(IDKY)
          DO 60 M=1,5
  60      MATCH(M)=.FALSE.
          DO 80 M=1,5
          DO 70 N=1,5
          IF (.NOT.MATCH(N).AND.IDKPRD(N,L).EQ.IDKPRD(M,J)) THEN
            MATCH(N)=.TRUE.
            GOTO 80
          ENDIF
  70      CONTINUE
  80      CONTINUE
          IF (MATCH(1).AND.MATCH(2).AND.MATCH(3).AND.
     &        MATCH(4).AND.MATCH(5)) GOTO 100
  90      L=LNEXT(L)
        ENDIF
        CALL HWDCHK(IDKY,J,*120)
        NMODES(IDKY)=NMODES(IDKY)+1
        IF (NMODES(IDKY).GT.NMXMOD) CALL HWWARN('HWUDKS',100,*999)
        LNEXT(LAST)=J
        BRSUM=BRSUM+BRFRAC(J)
        LTMP(NMODES(IDKY))=J
        BRTMP(NMODES(IDKY))=-BRFRAC(J)
        LAST=J
C Sets CMMOM(IDKY) = CoM momentum for next 2-body decay mode J (else 0)
        IF (NPRODS(J).EQ.2) CMMOM(J)=
     &   HWUPCM(RMASS(IDKY),RMASS(IDKPRD(1,J)),RMASS(IDKPRD(2,J)))
      ENDIF
      GOTO 120
  100 WRITE(6,110) L,J,BRFRAC(L),NME(L)
      BRFRAC(J)=BRFRAC(L)
      NME(J)=NME(L)
  110 FORMAT(1X,'Line ',I4,' is the same as line ',I4/
     &       1X,'Take BR ',F5.3,' and ME code ',I3,' from second entry')
  120 CONTINUE
C Set sum of branching ratios to 1. if necessary
      IF (ABS(BRSUM-1.).GT.EPS) THEN
        WRITE(6,130) RNAME(IDKY),BRSUM
  130   FORMAT(1X,A8,': BR sum =',F8.5)
        IF (ABS(BRSUM).LT.EPS) THEN
          WRITE(6,140)
  140     FORMAT(1X,'Setting particle stable'/)
          NMODES(IDKY)=0
        ELSE
          WRITE(6,150)
  150     FORMAT(1X,'Rescaling to 1'/)
          SCALE=1./BRSUM
          K=LSTRT(IDKY)
          DO 160 J=1,NMODES(IDKY)
          BRFRAC(K)=SCALE*BRFRAC(K)
  160     K=LNEXT(K)
        ENDIF
      ENDIF
C Sort branching ratios into descending order and rearrange pointers
      CALL HWUSOR(BRTMP,NMODES(IDKY),INDX,2)
      LSTRT(IDKY)=LTMP(INDX(1))
      LNEXT(LTMP(INDX(1)))=LTMP(INDX(1))
      DO 170 J=2,NMODES(IDKY)
  170 LNEXT(LTMP(INDX(J-1)))=LTMP(INDX(J))
      LNEXT(LTMP(INDX(NMODES(IDKY))))=LTMP(INDX(NMODES(IDKY)))
  180 CONTINUE
C If not a short lived particle with a decay mode then set stable
      DO 190 I=1,NRES
      IF (.NOT.RSTAB(I).AND.RLTIM(I).LT.PLTCUT.AND.
     &    (NMODES(I).GT.0.OR.
     &     (BPDK.AND.((I.GE.221.AND.I.LE.231).OR.
     &                (I.GE.245.AND.I.LE.254))))) THEN
        DKLTM(I)=RLTIM(I)*RMASS(I)/HBAR
      ELSE
        RSTAB(I)=.TRUE.
      ENDIF
  190 CONTINUE
C Set up DKLTM for light quarks
      DO 200 I=1,5
      DKLTM(I)=VMIN2/RMASS(I)**2
  200 DKLTM(I+6)=DKLTM(I)
C gluon
      DKLTM(13)=VMIN2/RMASS(13)**2
C and diquarks
      DO 210 I=109,114
      DKLTM(I)=VMIN2/RMASS(I)**2
  210 DKLTM(I+6)=DKLTM(I)
C Set up DKLTM for weak bosons
      DKLTM(198)=EXAG*RMASS(198)/GAMW
      DKLTM(199)=DKLTM(198)
      DKLTM(200)=EXAG*RMASS(200)/GAMZ
      DKLTM(201)=EXAG*RMASS(201)/GAMH
      DKLTM(202)=EXAG*RMASS(202)/GAMZP
C Set up DKTRM for massive quarks (plus check m_Q > M_W + m_q)
      FAC=EXAG*HWUAEM(RMASS(198)**2)/(FOUR*SCABI*RMASS(198))**2
      DKLTM(6)=FAC*FN(RMASS(6  ),RMASS(5  ),RMASS(198))
      DKLTM(12)=DKLTM(6)
      IF (RMASS(6).LT.RMASS(5)+RMASS(198))
     & WRITE(6,220) RNAME(6),RNAME(5),RNAME(198)
      DKLTM(209)=FAC*FN(RMASS(209),RMASS(4  ),RMASS(198))
      DKLTM(215)=DKLTM(209)
      IF (RMASS(209).LT.RMASS(4)+RMASS(198))
     & WRITE(6,220) RNAME(209),RNAME(4),RNAME(198)
      DKLTM(210)=FAC*FN(RMASS(210),RMASS(209),RMASS(198))
      DKLTM(216)=DKLTM(210)
      IF (RMASS(210).LT.RMASS(209)+RMASS(198))
     & WRITE(6,220) RNAME(210),RNAME(209),RNAME(198)
      DKLTM(211)=FAC*FN(RMASS(211),RMASS(6  ),RMASS(198))
      DKLTM(217)=DKLTM(211)
      IF (RMASS(211).LT.RMASS(6)+RMASS(198))
     & WRITE(6,220) RNAME(211),RNAME(6),RNAME(198)
      DKLTM(212)=FAC*FN(RMASS(212),RMASS(211),RMASS(198))
      DKLTM(218)=DKLTM(212)
      IF (RMASS(212).LT.RMASS(211)+RMASS(198))
     & WRITE(6,220) RNAME(212),RNAME(211),RNAME(198)
 220  FORMAT(1X,'W not real in the decay: ',A8,' --> ',A8,' + ',A8)
C Now carry out diagnostic checks on decay table
      CALL HWDTOP(TOPDKS)
      DO 310 IRES=1,NRES
      IAPDG=ABS(IDPDG(IRES))
C Do not check (di-)quarks, gauge bosons, higgses or special particles
      IF ((IAPDG.GE.1.AND.IAPDG.LE.9).OR.MOD(IAPDG/10,10).EQ.0.OR.
     &    (IAPDG.GE.21.AND.IAPDG.LE.25).OR.IAPDG.EQ.32.OR.
     &    (IAPDG.GE.35.AND.IAPDG.LE.37).OR.
     &    IAPDG.EQ.91.OR.IAPDG.EQ.9998.OR.IAPDG.EQ.9999) THEN
        GOTO 310
C Ignore top hadrons if top decays
      ELSEIF(TOPDKS.AND.((IRES.GE.232.AND.IRES.LE.244).OR.
     &                   (IRES.GE.255.AND.IRES.LE.264))) THEN
        GOTO 310
C Ignore particles not produced in cluster or particle decays
      ELSEIF(VTOCDK(IRES).AND.VTORDK(IRES)) THEN
        GOTO 310
C Ignore B's if EURO or CLEO decay package used
      ELSEIF(((IRES.GE.221.AND.IRES.LE.223).OR.
     &        (IRES.GE.245.AND.IRES.LE.247)).AND.BDECAY.NE.'HERW') THEN
        WRITE(6,320) BDECAY,RNAME(IRES)
C Check decay modes exist for massive, short lived particles
      ELSEIF (NMODES(IRES).EQ.0.AND.RMASS(IRES).NE.0..AND.
     &        RLTIM(IRES).LT.PLTCUT) THEN
        IF (VTOCDK(IRES)) THEN
          CVETO(1)='VETOED '
        ELSE
          CVETO(1)='ALLOWED'
        ENDIF
        IF (VTORDK(IRES)) THEN
          CVETO(2)='VETOED '
        ELSE
          CVETO(2)='ALLOWED'
        ENDIF
        WRITE(6,330) RNAME(IRES),CVETO(1),CVETO(2)
C ignore particles with no modes if massless or long lived
      ELSEIF (NMODES(IRES).EQ.0.AND.
     &        (RMASS(IRES).EQ.0..OR.RLTIM(IRES).GT.PLTCUT)) THEN
        GOTO 310
      ELSEIF (IDPDG(IRES).LT.0) THEN
C Antiparticle: check decays are charge conjugates of particle decays
        CALL HWUIDT(1,-IDPDG(IRES),IPART,CDUM)
        IF (NMODES(IPART).EQ.0) THEN
C Nothing to compare to
          WRITE(6,340) RNAME(IPART),RNAME(IRES)
        ELSE
C First initialize particle matching array
          DO 230 I=1,NMODES(IPART)
  230     PMATCH(I)=.FALSE.
C Loop through antiparticle decay modes
          LR=LSTRT(IRES)
          DO 290 I=1,NMODES(IRES)
C Search for conjugate mode allowing for different particle order
          LP=LSTRT(IPART)
          DO 270 J=1,NMODES(IPART)
          IF (PMATCH(J)) GOTO 270
          DO 240 K=1,5
  240     MATCH(K)=.FALSE.
          DO 260 K=1,5
          KPRDLR=HWUANT(IDKPRD(K,LR))
          DO 250 L=1,5
          IF (.NOT.MATCH(L).AND.KPRDLR.EQ.IDKPRD(L,LP) ) THEN
            MATCH(L)=.TRUE.
            GOTO 260
          ENDIF
  250     CONTINUE
  260     CONTINUE
          IF (MATCH(1).AND.MATCH(2).AND.MATCH(3).AND.
     &        MATCH(4).AND.MATCH(5)) GOTO 280
  270     LP=LNEXT(LP)
C No match found
          WRITE(6,350) LR,RNAME(IRES),(RNAME(IDKPRD(J,LR)),J=1,5)
          GOTO 290
C Match found, check branching ratios and matrix element codes
  280     PMATCH(J)=.TRUE.
          IF (BRFRAC(LR).NE.BRFRAC(LP))
     &     WRITE(6,360) LR,RNAME(IRES),(RNAME(IDKPRD(J,LR)),J=1,5),
     &                  BRFRAC(LR),BRFRAC(LP)
          IF (NME(LR).NE.NME(LP))
     &     WRITE(6,370) LR,RNAME(IRES),(RNAME(IDKPRD(J,LR)),J=1,5),
     &                  NME(LR),NME(LP)
  290     LR=LNEXT(LR)
C Check for unmatched modes of particle conjugate to antiparticle
          LP=LSTRT(IPART)
          DO 300 I=1,NMODES(IPART)
          IF (.NOT.PMATCH(I))
     &     WRITE(6,350) LP,RNAME(IPART),(RNAME(IDKPRD(J,LP)),J=1,5)
  300     LP=LNEXT(LP)
        ENDIF
      ENDIF
  310 CONTINUE
  320 FORMAT(1X,A8,' decay package to be used for particle ',A8)
  330 FORMAT(1X,'No decay modes available for particle ',A8/
     & 1X,'Production in cluster decays ',A7,' and particle decays ',A7)
  340 FORMAT(1X,A8,' has no modes conjugate to those of ',A8)
  350 FORMAT(1X,'Line, ',I4,' decay: ',A8,' --> ',4(A8,1X),A8/
     &       1X,'A charge conjugate decay mode does not exist')
  360 FORMAT(1X,'Line, ',I4,' decay: ',A8,' --> ',4(A8,1X),A8/
     &       1X,'BR ',F5.3,' unequal to that of conjugate mode ',F5.3)
  370 FORMAT(1X,'Line, ',I4,' decay: ',A8,' --> ',4(A8,1X),A8/
     &       1X,'ME code ',I3,' unequal to that of conjugate mode ',I3)
  999 RETURN
      END
