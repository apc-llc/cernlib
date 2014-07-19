CDECK  ID>, HWUANT.
*CMZ :-        -
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      FUNCTION HWUANT(IPART)
C-----------------------------------------------------------------------
C     Returns the antiparticle of IPART; uses HERWIG numbering
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER HWUANT,IPART,IPDG,IANTI,OLDERR
      CHARACTER*8 CDUM
      OLDERR=IERROR
      IPDG=IDPDG(IPART)
      IF (IPDG.EQ. 9.OR.IPDG.EQ.21.OR.IPDG.EQ.22.OR.IPDG.EQ.23.OR.
     &    IPDG.EQ.25.OR.IPDG.EQ.32.OR.IPDG.EQ.91.OR.IPDG.EQ.130.OR.
     &    IPDG.EQ.310.OR.(FLOAT(INT(RSPIN(IPART))).EQ.RSPIN(IPART).AND.
     &                    MOD(IPDG/100,10).EQ.MOD(IPDG/10,10))) THEN
C Self-conjugate boson
        IANTI=IPART
      ELSEIF(IPART.EQ.211.OR.IPART.EQ.212) THEN
        IANTI=IPART+6
      ELSEIF(IPART.EQ.217.OR.IPART.EQ.218) THEN
        IANTI=IPART-6
      ELSE
        CALL HWUIDT(1,-IPDG,IANTI,CDUM)
      ENDIF
      IF (IANTI.EQ.20) WRITE(6,10) RNAME(IPART)
  10  FORMAT(1X,A8,' has no antiparticle')
      HWUANT=IANTI
      IERROR=OLDERR
      END
