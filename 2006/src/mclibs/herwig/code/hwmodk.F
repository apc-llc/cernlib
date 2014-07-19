CDECK  ID>, HWMODK.
*CMZ :-        -
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWMODK(IDKTMP,BRTMP,IMETMP,
     & IATMP,IBTMP,ICTMP,IDTMP,IETMP)
C-----------------------------------------------------------------------
C     Takes the decay, IDKTMP -> I-(A+B+C+D+E)-TMP, and simply stores it
C     if internal pointers not set up (.NOT.DKPSET) else if pre-existing
C     mode updates branching ratio BRTMP and matrix element code IMETMP,
C     if -ve leaves as is. If a new mode adds to table and if consistent
C     adjusts pointers,  sets CMMOM (for two-body mode) and resets RSTAB
C     if necessary.  The branching ratios of any other IDKTMP decays are
C     scaled by (1.-BRTMP)/(1.-BR_OLD)
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWUPCM,BRTMP,SCALE,EPS
      INTEGER IDKTMP,IMETMP,IATMP,IBTMP,ICTMP,IDTMP,IETMP,IDKY,ITMP(5),
     & L,I,J,K,JPREV
      LOGICAL MATCH(5)
      CHARACTER*8 CDUM
      EXTERNAL HWUPCM
      PARAMETER (EPS=1.D-6)
C Convert to internal format
      CALL HWUIDT(1,IDKTMP,IDKY,CDUM)
      IF (IDKY.EQ.20) THEN
        WRITE(6,10) IDKTMP
  10    FORMAT(1X,'Particle decaying,',I7,', is not recognised')
        RETURN
      ENDIF
      CALL HWUIDT(1,IATMP,ITMP(1),CDUM)
      CALL HWUIDT(1,IBTMP,ITMP(2),CDUM)
      CALL HWUIDT(1,ICTMP,ITMP(3),CDUM)
      CALL HWUIDT(1,IDTMP,ITMP(4),CDUM)
      CALL HWUIDT(1,IETMP,ITMP(5),CDUM)
C If internal pointers not yet set up simply store decay
      IF (.NOT.DKPSET) THEN
        NDKYS=NDKYS+1
        IF (NDKYS.GT.NMXDKS) CALL HWWARN('HWMODK',100,*999)
        IDK(NDKYS)=IDKY
        BRFRAC(NDKYS)=BRTMP
        NME(NDKYS)=IMETMP
        DO 20 I=1,5
  20    IDKPRD(I,NDKYS)=ITMP(I)
      ELSE
        IF (NMODES(IDKY).GT.0) THEN
C First search to see if mode pre-exists
          IF ((ITMP(2).GE.1.AND.ITMP(2).LE.13).OR.
     &        (ITMP(3).GE.1.AND.ITMP(3).LE.13)) THEN
C Partonic respect order
            L=LSTRT(IDKY)
            DO 30 K=1,NMODES(IDKY)
                IF (ITMP(1).EQ.IDKPRD(1,L).AND.
     &              ITMP(2).EQ.IDKPRD(2,L).AND.
     &              ITMP(3).EQ.IDKPRD(3,L).AND.
     &              ITMP(4).EQ.IDKPRD(4,L).AND.
     &              ITMP(5).EQ.IDKPRD(5,L)) GOTO 90
  30        L=LNEXT(L)
          ELSE
C Allow for different order in matching
            L=LSTRT(IDKY)
            DO 70 I=1,NMODES(IDKY)
            DO 40 J=1,5
  40        MATCH(J)=.FALSE.
            DO 60 J=1,5
            DO 50 K=1,5
            IF (.NOT.MATCH(K).AND.ITMP(K).EQ.IDKPRD(J,L)) THEN
              MATCH(K)=.TRUE.
              GOTO 60
            ENDIF
  50        CONTINUE
  60        CONTINUE
            IF (MATCH(1).AND.MATCH(2).AND.MATCH(3).AND.
     &          MATCH(4).AND.MATCH(5)) GOTO 90
  70        L=LNEXT(L)
          ENDIF
        ENDIF
C A new mode put decay products in table
        NDKYS=NDKYS+1
        IF (NDKYS.GT.NMXDKS) CALL HWWARN('HWMODK',100,*999)
        DO 80 I=1,5
  80    IDKPRD(I,NDKYS)=ITMP(I)
C If decay consistent set up new pointers
        CALL HWDCHK(IDKY,NDKYS,*980)
        IF (NMODES(IDKY).EQ.0) THEN
          LSTRT(IDKY)=NDKYS
          IF (RLTIM(IDKY).LT.PLTCUT.AND.RMASS(IDKY).NE.0.) THEN
            RSTAB(IDKY)=.FALSE.
            DKLTM(IDKY)=RLTIM(IDKY)*RMASS(IDKY)/HBAR
          ELSE
            RSTAB(IDKY)=.TRUE.
          ENDIF
        ELSE
          LNEXT(L)=NDKYS
        ENDIF
        NMODES(IDKY)=NMODES(IDKY)+1
        LNEXT(NDKYS)=NDKYS
        L=NDKYS
C Set CMMOM if two body decay
        IF (NPRODS(L).EQ.2) CMMOM(L)=
     &   HWUPCM(RMASS(IDKY),RMASS(IDKPRD(1,L)),RMASS(IDKPRD(2,L)))
C A Pre-existing mode, line L, add/update ME code and BR, scaling all
C other branching fractions
  90    IF (IMETMP.GT.0) NME(L)=IMETMP
        IF (ABS(BRTMP-1.).LT.EPS) THEN
C This modes dominant: eliminate others
          NMODES(IDKY)=1
          LSTRT(IDKY)=L
          BRFRAC(L)=1.D0
          LNEXT(L)=L
        ELSEIF (ABS(BRTMP).LT.EPS) THEN
C This mode to be eliminated
          IF (NMODES(IDKY).EQ.1) THEN
            RSTAB(IDKY)=.TRUE.
          ELSE
            J=LSTRT(IDKY)
            IF (J.EQ.L) THEN
              LSTRT(IDKY)=LNEXT(J)
            ELSE
              JPREV=J
              DO 100 I=2,NMODES(IDKY)
              J=LNEXT(J)
              IF (J.EQ.L) LNEXT(JPREV)=LNEXT(J)
  100         JPREV=(J)
            ENDIF
          ENDIF
          NMODES(IDKY)=NMODES(IDKY)-1
        ELSE
C Rescale all other modes
          BRFRAC(L)=BRTMP
          IF (L.EQ.NDKYS) THEN
            SCALE=ONE-BRTMP
          ELSEIF (NMODES(IDKY).EQ.1) THEN
            BRFRAC(L)=ONE
          ELSE
            SCALE=(ONE-BRTMP)/(ONE-BRFRAC(L))
          ENDIF
          J=LSTRT(IDKY)
          DO 110 I=1,NMODES(IDKY)
          IF (J.NE.L) BRFRAC(J)=SCALE*BRFRAC(J)
  110     J=LNEXT(J)
        ENDIF
      ENDIF
      GOTO 999
  980 WRITE(6,990)
  990 FORMAT(1X,'Decay mode inconsistent, no modifications made')
  999 RETURN
      END
