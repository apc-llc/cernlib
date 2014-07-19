*
* $Id: pdfset.F,v 1.1.1.1 1996/03/08 17:02:19 mclareni Exp $
*
* $Log: pdfset.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:19  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, PDFSET.
*CMZ :-        -26/04/91  11.11.54  by  Bryan Webber
*-- Author :    Bryan Webber
C----------------------------------------------------------------------
      SUBROUTINE PDFSET(PARM,VAL)
C---DUMMY SUBROUTINE: DELETE AND SET MODPDF(I)
C   IN MAIN PROGRAM IF YOU USE PDFLIB CERN-LIBRARY
C   PACKAGE FOR NUCLEON STRUCTURE FUNCTIONS
      DOUBLE PRECISION VAL(10)
      CHARACTER*20 PARM(10)
      WRITE (6,10)
   10 FORMAT(/10X,'PDFSET CALLED BUT NOT LINKED')
      STOP
      END
