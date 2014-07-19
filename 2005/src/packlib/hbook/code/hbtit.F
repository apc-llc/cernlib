*
* $Id: hbtit.F,v 1.1.1.1 1996/01/16 17:07:33 mclareni Exp $
*
* $Log: hbtit.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:33  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.46  by  Rene Brun
*-- Author :
      SUBROUTINE HBTIT(CHTITL,NWTITL,NCHT)
*.==========>
*.           computes number of words given in STRING CHTITL
*..=========> ( R.Brun )
      CHARACTER*(*) CHTITL
*.___________________________________________
      NCHT=LENOCC(CHTITL)
      IF(CHTITL(NCHT:NCHT).EQ.'$')NCHT=NCHT-1
      IF(NCHT.EQ.1)THEN
         IF(CHTITL(1:1).EQ.' ')NCHT=0
      ENDIF
      NWTITL=(NCHT+3)/4
*
  99  RETURN
      END
