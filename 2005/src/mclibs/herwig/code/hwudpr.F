CDECK  ID>, HWUDPR.
*CMZ :-        -
*-- Author :    Ian Knowles & Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWUDPR
C-----------------------------------------------------------------------
C     Prints out particle properies/decay tables
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER I,NM,J,K,L
C---Print out table of particle properties
      WRITE (6,10) NRES
  10  FORMAT(1H1//30X,'TABLE OF PROPERTIES OF',I4,' PARTICLES USED'//
     &/5X,'Name IDPDG    Mass   Chg Spn Lifetime  Modes',
     &' Branching fractions ME codes and decay products')
      DO 60 I=1,NRES
      IF (VTOCDK(I).AND.VTORDK(I)) GOTO 60
      IF (NMODES(I).EQ.0) THEN
        WRITE (6,20) I,RNAME(I),IDPDG(I),RMASS(I),ICHRG(I),RSPIN(I),
     &               RLTIM(I),0
  20    FORMAT(/I4,1X,A8,1X,I8,1X,F7.3,1X,I2,1X,F3.1,1X,E9.3,1X,I3)
      ELSE
C and any decay modes
        IF (RSTAB(I)) THEN
          NM=0
        ELSEIF (VTOCDK(I)) THEN
          NM=-NMODES(I)
        ELSE
          NM=NMODES(I)
        ENDIF
        K=LSTRT(I)
        WRITE (6,30) I,RNAME(I),IDPDG(I),RMASS(I),ICHRG(I),RSPIN(I),
     &      RLTIM(I),NM,BRFRAC(K),NME(K),(RNAME(IDKPRD(L,K)),L=1,5)
  30    FORMAT(/I4,1X,A8,1X,I8,1X,F7.3,1X,I2,1X,F3.1,1X,E9.3,1X,I3,2X,
     &         F5.3,1X,I3,5(1X,A8))
        IF (NMODES(I).GE.2) THEN
          DO 40 J=2,NMODES(I)
          K=LNEXT(K)
  40      WRITE(6,50) BRFRAC(K),NME(K),(RNAME(IDKPRD(L,K)),L=1,5)
  50      FORMAT(53X,F5.3,1X,I3,5(1X,A8))
        ENDIF
      ENDIF
  60  CONTINUE
      END
