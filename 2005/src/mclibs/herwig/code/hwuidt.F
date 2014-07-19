CDECK  ID>, HWUIDT.
*CMZ :-        -26/04/91  10.18.58  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWUIDT(IOPT,IPDG,IWIG,NWIG)
C-----------------------------------------------------------------------
C     TRANSLATES PARTICLE IDENTIFIERS:
C     IPDG = PARTICLE DATA GROUP CODE
C     IWIG = HERWIG IDENTITY CODE
C     NWIG = HERWIG CHARACTER*8 NAME
C
C     IOPT= 1 GIVEN IPDG, RETURNS IWIG AND NWIG
C     IOPT= 2 GIVEN IWIG, RETURNS IPDG AND NWIG
C     IOPT= 3 GIVEN NWIG, RETURNS IPDG AND IWIG
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER IOPT,IPDG,IWIG,I
      CHARACTER*8 NWIG
      IF (IOPT.EQ.1) THEN
        DO 10 I=0,NRES
        IF (IDPDG(I).EQ.IPDG) THEN
          IWIG=I
          NWIG=RNAME(I)
          RETURN
        ENDIF
  10    CONTINUE
        WRITE(6,20) IPDG
  20    FORMAT(1X,'Particle not recognised, PDG code: ',I7)
        IWIG=20
        NWIG=RNAME(20)
        CALL HWWARN('HWUIDT',101,*999)
      ELSEIF (IOPT.EQ.2) THEN
        IF (IWIG.LT.0.OR.IWIG.GT.NRES) THEN
          WRITE(6,30) IWIG
  30      FORMAT(1X,'Particle not recognised, HERWIG code: ',I3)
          IPDG=0
          NWIG=RNAME(20)
          CALL HWWARN('HWUIDT',102,*999)
        ELSE
          IPDG=IDPDG(IWIG)
          NWIG=RNAME(IWIG)
          RETURN
        ENDIF
      ELSEIF (IOPT.EQ.3) THEN
        DO 40 I=0,NRES
        IF (RNAME(I).EQ.NWIG) THEN
          IWIG=I
          IPDG=IDPDG(I)
          RETURN
        ENDIF
  40    CONTINUE
        WRITE(6,50) NWIG
  50    FORMAT(1X,'Particle not recognised, HERWIG name: ',A8)
        IWIG=20
        IPDG=0
        CALL HWWARN('HWUIDT',103,*999)
      ELSE
        CALL HWWARN('HWUIDT',404,*999)
      ENDIF
  999 END
