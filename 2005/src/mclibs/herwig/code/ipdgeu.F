CDECK  ID>, IPDGEU.
*CMZ :-        -28/01/92  12.34.44  by  Mike Seymour
*-- Author :    Luca Stanco
C-----------------------------------------------------------------------
      FUNCTION IPDGEU(I)
C-----------------------------------------------------------------------
C     DUMMY SUBROUTINE: DELETE AND SET BDECAY='EURO'
C     IN MAIN PROGRAM IF YOU USE EURODEC DECAY PACKAGE
C-----------------------------------------------------------------------
      INTEGER IPDGEU,I
      WRITE (6,10)
   10 FORMAT(/10X,'IPDGEU CALLED BUT NOT LINKED')
      IPDGEU=0
      STOP
      END
