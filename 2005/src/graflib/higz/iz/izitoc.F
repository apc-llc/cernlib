*
* $Id: izitoc.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izitoc.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.11/00 27/09/90  15.56.02  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IZITOC(IVAL,CVAL)
*.===========>
*.
*. Convert integer value IVAL into character string CVAL
*.
*..==========>
      CHARACTER*(*) CVAL
      CHARACTER*12 CODED
*.______________________________________
*
      IF1=1
      IIVAL=IVAL
 10   IIVAL=IIVAL/10
      IF(IIVAL.NE.0)THEN
         IF1=IF1+1
         GOTO 10
      ENDIF
      IF(IVAL.LT.0)THEN
         IF1=IF1+1
      ENDIF
      WRITE(CODED,1000)IF1
      WRITE(CVAL,CODED)IVAL
*
 1000 FORMAT(2H(I,I2,1H))
      END
