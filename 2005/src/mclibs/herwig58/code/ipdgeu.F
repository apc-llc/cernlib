*
* $Id: ipdgeu.F,v 1.1.1.1 1996/03/08 17:02:19 mclareni Exp $
*
* $Log: ipdgeu.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:19  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, IPDGEU.
*CMZ :-        -28/01/92  12.34.44  by  Mike Seymour
*-- Author :    Luca Stanco
C-----------------------------------------------------------------------
      FUNCTION IPDGEU(I)
C---DUMMY SUBROUTINE: DELETE AND SET BDECAY='EURO'
C   IN MAIN PROGRAM IF YOU USE EURODEC DECAY PACKAGE
      INTEGER IPDGEU,I
      WRITE (6,10)
   10 FORMAT(/10X,'IPDGEU CALLED BUT NOT LINKED')
      IPDGEU=0
      STOP
      END
