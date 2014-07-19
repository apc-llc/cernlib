*
* $Id: hvhbvi.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: hvhbvi.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.45  by  Unknown
*-- Author :
CDECK  ID>, HVHBVI.
*CMZ :-        -28/01/92  12.34.44  by  Mike Seymour
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HVHBVI
C---DUMMY ROUTINE: DELETE IF YOU LINK TO BARYON NUMBER VIOLATION PACKAGE
      WRITE (6,10)
   10 FORMAT(/10X,'HERBVI CALLED BUT NOT LINKED')
      STOP
      END
