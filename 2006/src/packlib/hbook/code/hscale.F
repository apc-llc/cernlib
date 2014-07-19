*
* $Id: hscale.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hscale.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  14.38.24  by  Rene Brun
*-- Author :
      SUBROUTINE HSCALE(ID2,SCALE)
*.==========>
*.           STORE SCALE FACTOR FOR SCATTER-PLOTS
*..=========> ( R.Brun )
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
*.___________________________________________
      IRET=3
      NARG=2
      CALL NOARG(NARG)
      SCAL=0.
      IF(NARG.EQ.2)SCAL=FLOARG(SCALE)
*
  10  CALL HLOOP(ID2,'HSCALE',IRET)
      IF(IRET.EQ.0)GO TO 99
*
      CALL HDCOFL
      IF(I230.NE.0)THEN
         Q(LCID+KSCAL2)=SCAL
         CALL HSIFLA(19,1)
      ENDIF
      IRET=2
      GO TO 10
*
  99  RETURN
      END
