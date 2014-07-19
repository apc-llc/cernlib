CDECK  ID>, PDFSET.
*CMZ :-        -26/04/91  11.11.54  by  Bryan Webber
*-- Author :    Bryan Webber
C----------------------------------------------------------------------
      SUBROUTINE PDFSET(PARM,VAL)
C----------------------------------------------------------------------
C     DUMMY SUBROUTINE: DELETE AND SET MODPDF(I)
C     IN MAIN PROGRAM IF YOU USE PDFLIB CERN-LIBRARY
C     PACKAGE FOR NUCLEON STRUCTURE FUNCTIONS
C----------------------------------------------------------------------
      DOUBLE PRECISION VAL(20)
      CHARACTER*20 PARM(20)
      WRITE (6,10)
   10 FORMAT(/10X,'PDFSET CALLED BUT NOT LINKED')
      STOP
      END
