CDECK  ID>, HWUSTA.
*CMZ :-        -26/04/91  10.18.58  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWUSTA(NAME)
C-----------------------------------------------------------------------
C     MAKES PARTICLE TYPE 'NAME' STABLE
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER IPDG,IWIG
      CHARACTER*8 NAME
      CALL HWUIDT(3,IPDG,IWIG,NAME)
      IF (IWIG.EQ.20) CALL HWWARN('HWUSTA',500,*999)
      RSTAB(IWIG)=.TRUE.
      WRITE (6,10) IWIG,NAME
   10 FORMAT(/10X,'PARTICLE TYPE',I4,'=',A8,' SET STABLE')
  999 END
