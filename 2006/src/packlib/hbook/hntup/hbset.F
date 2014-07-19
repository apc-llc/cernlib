*
* $Id: hbset.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hbset.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 20/12/93  18.16.58  by  Fons Rademakers
*-- Author :    Fons Rademakers   02/06/92
      SUBROUTINE HBSET(OPTION, IVAL, IERROR)
*.==========>
*.           N-tuple option setting
*.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcset.inc"
*
      CHARACTER*(*) OPTION
      CHARACTER*8   OPT
*
      IERROR = 0
*
      OPT = OPTION
      CALL CLTOU(OPT)
*
      IF (OPT .EQ. 'BSIZE') THEN
         IBSIZE = IVAL
      ELSE
         CALL HBUG('Illegal option','HBSET',0)
         IERROR = 1
      ENDIF
*
      END
