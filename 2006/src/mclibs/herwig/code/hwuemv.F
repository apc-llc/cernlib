CDECK  ID>, HWUEMV.
*CMZ :-        -30/06/94  19.31.08  by  Mike Seymour
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWUEMV(N,IFROM,ITO)
C-----------------------------------------------------------------------
C     MOVE A BLOCK OF ENTRIES IN THE EVENT RECORD
C     N ENTRIES IN HEPEVT STARTING AT IFROM ARE MOVED TO AFTER ITO
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER N,IFROM,ITO,IMAP(0:NMXHEP),LFROM,LTO,I,IEDT(NMXHEP),IHEP,
     $ JHEP,KHEP
      COMMON /HWUMAP/IMAP
      LFROM=IFROM
      LTO=ITO
      DO 100 I=1,N
 100  IEDT(I)=LTO
      CALL HWUEDT(-N,IEDT)
      DO 300 I=1,N
        IHEP=LTO+I
        JHEP=IMAP(LFROM+I-1)
        ISTHEP(IHEP)=ISTHEP(JHEP)
        IDHW(IHEP)=IDHW(JHEP)
        IDHEP(IHEP)=IDHEP(JHEP)
        JMOHEP(1,IHEP)=JMOHEP(1,JHEP)
        JMOHEP(2,IHEP)=JMOHEP(2,JHEP)
        JDAHEP(1,IHEP)=JDAHEP(1,JHEP)
        JDAHEP(2,IHEP)=JDAHEP(2,JHEP)
        CALL HWVEQU(5,PHEP(1,JHEP),PHEP(1,IHEP))
        CALL HWVEQU(4,VHEP(1,JHEP),VHEP(1,IHEP))
        DO 200 KHEP=1,NHEP
          IF (JMOHEP(1,KHEP).EQ.JHEP) JMOHEP(1,KHEP)=IHEP
          IF (JMOHEP(2,KHEP).EQ.JHEP) JMOHEP(2,KHEP)=IHEP
          IF (JDAHEP(1,KHEP).EQ.JHEP) JDAHEP(1,KHEP)=IHEP
          IF (JDAHEP(2,KHEP).EQ.JHEP) JDAHEP(2,KHEP)=IHEP
 200    CONTINUE
        IEDT(I)=JHEP
 300  CONTINUE
      CALL HWUEDT(N,IEDT)
 999  END
