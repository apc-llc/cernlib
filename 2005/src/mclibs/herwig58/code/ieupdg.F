*
* $Id: ieupdg.F,v 1.1.1.1 1996/03/08 17:02:19 mclareni Exp $
*
* $Log: ieupdg.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:19  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, IEUPDG.
*CMZ :-        -28/01/92  12.34.44  by  Mike Seymour
*-- Author :    Luca Stanco
C-----------------------------------------------------------------------
      FUNCTION IEUPDG(I)
C---DUMMY SUBROUTINE: DELETE AND SET BDECAY='EURO'
C   IN MAIN PROGRAM IF YOU USE EURODEC DECAY PACKAGE
      INTEGER IEUPDG,I
      WRITE (6,10)
   10 FORMAT(/10X,'IEUPDG CALLED BUT NOT LINKED')
      IEUPDG=0
      STOP
      END
