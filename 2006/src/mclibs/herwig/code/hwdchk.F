CDECK  ID>, HWDCHK.
*CMZ :-        -
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWDCHK(IDKY,L,*)
C-----------------------------------------------------------------------
C     Checks line L of decay table is compatible with decay of particle
C     IDKY, tidies up the line and sets NPRODS.
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION EPS,QS,Q,DM
      INTEGER IDKY,L,IFAULT,I,ID,J
      PARAMETER (EPS=1.D-6)
      IF (VTOCDK(IDKY).AND.VTORDK(IDKY)) RETURN 1
      IFAULT=0
      QS=FLOAT(ICHRG(IDKY))
      IF (IDKY.LE.12.OR.(IDKY.GE.209.AND.IDKY.LE.220)) QS=QS/3.
      DM=RMASS(IDKY)
      NPRODS(L)=0
      DO 10 I=1,5
      ID=IDKPRD(I,L)
      IF (ID.LT.0.OR.ID.EQ.20.OR.ID.GT.NRES) THEN
        WRITE(6,20) L,RNAME(IDKY),(RNAME(IDKPRD(J,L)),J=1,5)
        IFAULT=IFAULT+1
      ELSEIF (ID.NE.0) THEN
        IF (VTORDK(ID)) THEN
          WRITE(6,30) L,RNAME(IDKY),(RNAME(IDKPRD(J,L)),J=1,5),RNAME(ID)
          IFAULT=IFAULT+1
        ENDIF
        NPRODS(L)=NPRODS(L)+1
        IDKPRD(NPRODS(L),L)=ID
        Q=FLOAT(ICHRG(ID))
        IF (ID.LE.12.OR.(ID.GE.109.AND.ID.LE.120).OR.
     &                  (ID.GE.209.AND.ID.LE.220)) Q=Q/3.
        QS=QS-Q
        DM=DM-RMASS(ID)
      ENDIF
  10  CONTINUE
C print any warnings
      IF (NPRODS(L).EQ.0) THEN
        WRITE(6,20) L,RNAME(IDKY),(RNAME(IDKPRD(I,L)),I=1,5)
        IFAULT=IFAULT+1
      ELSE
        IF (ABS(QS).GT.EPS) THEN
          WRITE(6,40) L,RNAME(IDKY),(RNAME(IDKPRD(I,L)),I=1,5),QS
          IFAULT=IFAULT+1
        ENDIF
        IF (DM.LT.0.) THEN
          WRITE(6,50) L,RNAME(IDKY),(RNAME(IDKPRD(I,L)),I=1,5),DM
          IFAULT=IFAULT+1
        ENDIF
      ENDIF
  20  FORMAT(1X,'Line, ',I4,' decay: ',A8,' --> ',4(A8,1X),A8/
     &       1X,'contains no or unrecognised decay product(s)')
  30  FORMAT(1X,'Line, ',I4,' decay: ',A8,' --> ',4(A8,1X),A8/
     &       1X,'contains decay product ',A8,' which is vetoed')
  40  FORMAT(1X,'Line, ',I4,' decay: ',A8,' --> ',4(A8,1X),A8/
     &       1X,'violates charge conservation, Qin-Qout= ',F6.3)
  50  FORMAT(1X,'Line, ',I4,' decay: ',A8,' --> ',4(A8,1X),A8/
     &       1X,'is kinematically not allowed, Min-Mout= ',F10.3)
      IF (IFAULT.NE.0) THEN
        RETURN 1
      ELSE
        RETURN
      ENDIF
      END
