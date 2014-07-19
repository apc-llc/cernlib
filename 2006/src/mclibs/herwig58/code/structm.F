*
* $Id: structm.F,v 1.1.1.1 1996/03/08 17:02:19 mclareni Exp $
*
* $Log: structm.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:19  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, STRUCTM.
*CMZ :S        E26/04/91  11.11.54  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE STRUCTM(X,QSCA,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)
C---DUMMY SUBROUTINE: DELETE IF YOU USE PDFLIB CERN-LIBRARY
C   PACKAGE FOR NUCLEON STRUCTURE FUNCTIONS
      DOUBLE PRECISION X,QSCA,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU
      WRITE (6,10)
   10 FORMAT(/10X,'STRUCTM CALLED BUT NOT LINKED')
      STOP
      END
