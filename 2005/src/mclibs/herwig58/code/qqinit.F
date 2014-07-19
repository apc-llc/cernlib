*
* $Id: qqinit.F,v 1.1.1.1 1996/03/08 17:02:19 mclareni Exp $
*
* $Log: qqinit.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:19  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, QQINIT.
*CMZ :-        -28/01/92  12.34.44  by  Mike Seymour
*-- Author :    Luca Stanco
C-----------------------------------------------------------------------
      SUBROUTINE QQINIT(QQLERR)
C---DUMMY SUBROUTINE: DELETE AND SET BDECAY='CLEO'
C   IN MAIN PROGRAM IF YOU USE CLEO DECAY PACKAGE
      LOGICAL QQLERR
      WRITE (6,10)
   10 FORMAT(/10X,'QQINIT CALLED BUT NOT LINKED')
      STOP
      END
