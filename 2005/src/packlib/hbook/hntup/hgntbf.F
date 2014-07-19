*
* $Id: hgntbf.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hgntbf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/09 21/07/94  16.19.59  by  Fons Rademakers
*-- Author :    Fons Rademakers   29/01/92
      SUBROUTINE HGNTBF(IDN,VAR,IOFFST,NVAR,IDNEVT,IERROR)
*.==========>
*.
*.           Return in the preset addresses (set by HBNAME) or
*.           in the dynamically allocated buffers accessible
*.           via offsets iq(ioffst(i)+idnevt) the variable
*.           VAR(NVAR) of event IDNEVT.
*.
*..=========> ( A.A.Rademakers )
*
      CHARACTER*(*) VAR(*)
      INTEGER       IOFFST(*)
*
      NVAR1 = -NVAR
      CALL HGNT1(IDN, '*', VAR, IOFFST, NVAR1, IDNEVT, IERROR)
*
      END
