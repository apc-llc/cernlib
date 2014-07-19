CDECK  ID>, IEUPDG.
*CMZ :-        -28/01/92  12.34.44  by  Mike Seymour
*-- Author :    Luca Stanco
C-----------------------------------------------------------------------
      FUNCTION IEUPDG(I)
C-----------------------------------------------------------------------
C     DUMMY SUBROUTINE: DELETE AND SET BDECAY='EURO'
C     IN MAIN PROGRAM IF YOU USE EURODEC DECAY PACKAGE
C-----------------------------------------------------------------------
      INTEGER IEUPDG,I
      WRITE (6,10)
   10 FORMAT(/10X,'IEUPDG CALLED BUT NOT LINKED')
      IEUPDG=0
      STOP
      END
