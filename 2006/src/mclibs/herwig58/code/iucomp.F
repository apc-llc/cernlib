*
* $Id: iucomp.F,v 1.1.1.1 1996/03/08 17:02:19 mclareni Exp $
*
* $Log: iucomp.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:19  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, IUCOMP.
*CMZ :-        -28/01/92  12.34.44  by  Mike Seymour
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION IUCOMP(I,J,K)
C---DUMMY FUNCTION: : DELETE AND SET BDECAY='CLEO'
C   IN MAIN PROGRAM IF YOU USE CLEO DECAY PACKAGE
      INTEGER IUCOMP,I,J,K
      IUCOMP=0
      WRITE (6,10)
   10 FORMAT(/10X,'IUCOMP CALLED BUT NOT LINKED')
      STOP
      END
