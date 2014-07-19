*
* $Id: izctoi.F,v 1.1.1.1 1996/02/14 13:11:09 mclareni Exp $
*
* $Log: izctoi.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:09  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/07 05/04/89  10.01.28  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IZCTOI(CVAL,IVAL)
*.===========>
*.
*. Convert character string CVAL into integer value IVAL
*.
*..==========>
#include "higz/hiques.inc"
      CHARACTER*(*) CVAL
      PARAMETER (MAXDIG=15)
      CHARACTER*(MAXDIG) CHTEMP,BLANK
*.______________________________________
*
      IQUEST(1)=0
      L=LENOCC(CVAL)
      IF (L.EQ.0)RETURN
      IF (L.GT.MAXDIG) THEN
         IQUEST(1)=1
         RETURN
      ENDIF
      BLANK=' '
      IF (L.EQ.MAXDIG) THEN
         CHTEMP=CVAL
      ELSE
         CHTEMP=BLANK(:MAXDIG-L)//CVAL
      ENDIF
      READ (CHTEMP,'(I15)',ERR=10 ) IVAL
      RETURN
   10 IQUEST(1)=1
*
      END
