CDECK  ID>, HWUEDT.
*CMZ :-        -09/12/91  12.07.08  by  Mike Seymour
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWUEDT(N,IEDT)
C-----------------------------------------------------------------------
C     EDIT THE EVENT RECORD
C     IF N>0 DELETE THE N ENTRIES IN IEDT FROM EVENT RECORD
C     IF N<0 INSERT LINES AFTER THE -N ENTRIES IN IEDT
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER N,IEDT(*),IMAP(0:NMXHEP),IHEP,I,J,I1,I2
      COMMON /HWUMAP/IMAP
C---MOVE ENTRIES AND CALCULATE MAPPING OF POINTERS
      IF (N.EQ.0) THEN
        RETURN
      ELSEIF (N.GT.0) THEN
        I=1
        I1=1
        I2=NHEP
      ELSE
        I=NHEP-N
        I1=NHEP
        I2=1
      ENDIF
      DO 110 IHEP=I1,I2,SIGN(1,I2-I1)
        IMAP(IHEP)=I
        DO 100 J=1,ABS(N)
          IF (IHEP.EQ.IEDT(J)) THEN
            IF (N.GT.0) IMAP(IHEP)=0
            I=I-1
            IF (N.LT.0) IMAP(IHEP)=I
          ENDIF
 100    CONTINUE
        IF (IMAP(IHEP).EQ.I .AND. IHEP.NE.I) THEN
          ISTHEP(I)=ISTHEP(IHEP)
          IDHW(I)=IDHW(IHEP)
          IDHEP(I)=IDHEP(IHEP)
          JMOHEP(1,I)=JMOHEP(1,IHEP)
          JMOHEP(2,I)=JMOHEP(2,IHEP)
          JDAHEP(1,I)=JDAHEP(1,IHEP)
          JDAHEP(2,I)=JDAHEP(2,IHEP)
          CALL HWVEQU(5,PHEP(1,IHEP),PHEP(1,I))
          CALL HWVEQU(4,VHEP(1,IHEP),VHEP(1,I))
          ISTHEP(IHEP)=0
          IDHW(IHEP)=20
          IDHEP(IHEP)=0
          JMOHEP(1,IHEP)=0
          JMOHEP(2,IHEP)=0
          JDAHEP(1,IHEP)=0
          JDAHEP(2,IHEP)=0
          CALL HWVZRO(5,PHEP(1,IHEP))
          CALL HWVZRO(4,VHEP(1,IHEP))
        ENDIF
        I=I+SIGN(1,N)
 110  CONTINUE
      NHEP=NHEP-N
C---RELABEL POINTERS, SETTING ANY WHICH WERE TO DELETED ENTRIES TO ZERO
      IMAP(0)=0
      DO 200 IHEP=1,NHEP
        JMOHEP(1,IHEP)=IMAP(JMOHEP(1,IHEP))
        JMOHEP(2,IHEP)=IMAP(JMOHEP(2,IHEP))
        JDAHEP(1,IHEP)=IMAP(JDAHEP(1,IHEP))
        JDAHEP(2,IHEP)=IMAP(JDAHEP(2,IHEP))
 200  CONTINUE
      END
