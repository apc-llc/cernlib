*
* $Id: eudini.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: eudini.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.45  by  Unknown
*-- Author :
CDECK  ID>, EUDINI.
*CMZ :-        -28/01/92  12.34.44  by  Mike Seymour
*-- Author :    Luca Stanco
C-----------------------------------------------------------------------
      SUBROUTINE EUDINI
C---DUMMY SUBROUTINE: DELETE AND SET BDECAY='EURO'
C   IN MAIN PROGRAM IF YOU USE EURODEC DECAY PACKAGE
      WRITE (6,10)
   10 FORMAT(/10X,'EUDINI CALLED BUT NOT LINKED')
      STOP
      END
