CDECK  ID>, FRAGMT.
*CMZ :-        -28/01/92  12.34.44  by  Mike Seymour
*-- Author :    Luca Stanco
C-----------------------------------------------------------------------
      SUBROUTINE FRAGMT(I,J,K)
C-----------------------------------------------------------------------
C     DUMMY SUBROUTINE: DELETE AND SET BDECAY='EURO'
C     IN MAIN PROGRAM IF YOU USE EURODEC DECAY PACKAGE
C-----------------------------------------------------------------------
      INTEGER I,J,K
      WRITE (6,10)
   10 FORMAT(/10X,'FRAGMT CALLED BUT NOT LINKED')
      STOP
      END
