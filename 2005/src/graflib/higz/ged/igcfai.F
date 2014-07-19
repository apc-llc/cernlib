*
* $Id: igcfai.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: igcfai.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 16/12/88  14.00.46  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGCFAI(IVAL,CHVALI)
*.===========>
*.
*..==========> (O.Couet)
      CHARACTER*(*) CHVALI
*.______________________________________
*
      IF(IVAL.EQ.1)CHVALI='Solid'
      IF(IVAL.EQ.0)CHVALI='Hollow'
      IF(IVAL.EQ.2)CHVALI='Pattern'
      IF(IVAL.EQ.3)CHVALI='Hatch'
*
      END
