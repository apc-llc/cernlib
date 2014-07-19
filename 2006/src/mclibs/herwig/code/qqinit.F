CDECK  ID>, QQINIT.
*CMZ :-        -28/01/92  12.34.44  by  Mike Seymour
*-- Author :    Luca Stanco
C-----------------------------------------------------------------------
      SUBROUTINE QQINIT(QQLERR)
C-----------------------------------------------------------------------
C     DUMMY SUBROUTINE: DELETE AND SET BDECAY='CLEO'
C     IN MAIN PROGRAM IF YOU USE CLEO DECAY PACKAGE
C-----------------------------------------------------------------------
      LOGICAL QQLERR
      WRITE (6,10)
   10 FORMAT(/10X,'QQINIT CALLED BUT NOT LINKED')
      STOP
      END
