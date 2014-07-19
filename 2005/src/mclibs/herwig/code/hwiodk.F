CDECK  ID>, HWIODK.
*CMZ :-        -
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWIODK(IUNIT,IOPT,IME)
C-----------------------------------------------------------------------
C     If IUNIT > 0 writes out present HERWIG decay tables to unit IUNIT
C              < 0 reads in decay tables from unit IUNIT
C     The format used during the read/write is specified by IOPT
C     =1 PDG; =2 HERWIG numeric; =3 HERWIG character name.
C     When reading in if IME =1 matrix element codes >= 100 are accepted
C                             0                            are set zero.
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER IUNIT,IOPT,IME,JUNIT,I,J,K,L,IDKY,ITMP(5),IDUM
      CHARACTER*8 CDK(NMXDKS),CDKPRD(5,NMXDKS),CDUM
      JUNIT=ABS(IUNIT)
      OPEN(UNIT=JUNIT,FORM='FORMATTED',STATUS='UNKNOWN')
      IF (IUNIT.GT.0) THEN
C Write out the decay table
        WRITE(JUNIT,100) NDKYS
        IF (IOPT.EQ.1) THEN
          DO 20 I=1,NRES
          IF (NMODES(I).EQ.0) GOTO 20
          K=LSTRT(I)
          DO 10 J=1,NMODES(I)
          WRITE(JUNIT,110) IDPDG(I),BRFRAC(K),NME(K),
     &                    (IDPDG(IDKPRD(L,K)),L=1,5)
  10      K=LNEXT(K)
  20      CONTINUE
        ELSEIF (IOPT.EQ.2) THEN
          DO 40 I=1,NRES
          IF (NMODES(I).EQ.0) GOTO 40
          K=LSTRT(I)
          DO 30 J=1,NMODES(I)
          WRITE(JUNIT,120) I,BRFRAC(K),NME(K),(IDKPRD(L,K),L=1,5)
  30      K=LNEXT(K)
  40      CONTINUE
        ELSEIF (IOPT.EQ.3) THEN
          DO 60 I=1,NRES
          IF (NMODES(I).EQ.0) GOTO 60
          K=LSTRT(I)
          DO 50 J=1,NMODES(I)
          WRITE(JUNIT,130) RNAME(I),BRFRAC(K),NME(K),
     &                    (RNAME(IDKPRD(L,K)),L=1,5)
  50      K=LNEXT(K)
  60      CONTINUE
        ENDIF
      ELSEIF (IUNIT.LT.0) THEN
C Read in the decay table and convert to HERWIG numeric format
        READ(JUNIT,100) NDKYS
        IF (NDKYS.GT.NMXDKS) CALL HWWARN('HWIODK',100,*999)
        IF (IOPT.EQ.1) THEN
          DO 70 I=1,NDKYS
          READ(JUNIT,110) IDKY,BRFRAC(I),NME(I),ITMP
          IF (IME.EQ.0.AND.NME(I).GE.100) NME(I)=0
          CALL HWUIDT(1,IDKY,IDK(I),CDUM)
          DO 70 J=1,5
  70      CALL HWUIDT(1,ITMP(J),IDKPRD(J,I),CDUM)
        ELSEIF (IOPT.EQ.2) THEN
          DO 80 I=1,NDKYS
          READ(JUNIT,120) IDK(I),BRFRAC(I),NME(I),(IDKPRD(J,I),J=1,5)
          IF (IDK(I).LT.0.OR.IDK(I).GT.NRES) IDK(I)=20
  80      IF (IME.EQ.0.AND.NME(I).GE.100) NME(I)=0
        ELSEIF (IOPT.EQ.3) THEN
          DO 90 I=1,NDKYS
          READ(JUNIT,130) CDK(I),BRFRAC(I),NME(I),(CDKPRD(J,I),J=1,5)
          IF (IME.EQ.0.AND.NME(I).GE.100) NME(I)=0
          CALL HWUIDT(3,IDUM,IDK(I),CDK(I))
          DO 90 J=1,5
  90      CALL HWUIDT(3,IDUM,IDKPRD(J,I),CDKPRD(J,I))
        ELSE
          CALL HWWARN('HWIODK',101,*999)
        ENDIF
      ENDIF
      CLOSE(UNIT=JUNIT)
  100 FORMAT(1X,I4)
  110 FORMAT(1X,I7,1X,F7.5,1X,I3,5(1X,I7))
  120 FORMAT(1X,I3,1X,F7.5,6(1X,I3))
  130 FORMAT(1X,A8,1X,F7.5,1X,I3,5(1X,A8))
  999 RETURN
      END
