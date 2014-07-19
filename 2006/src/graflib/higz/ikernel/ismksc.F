*
* $Id: ismksc.F,v 1.2 1998/01/28 14:34:56 couet Exp $
*
* $Log: ismksc.F,v $
* Revision 1.2  1998/01/28 14:34:56  couet
* - GGDM driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:57  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/01 05/05/93  14.09.03  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISMKSC(RMSCF)
      ENTRY      GSMKSC(RMSCF)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
      IF(RMSCF.EQ.RMKSC)RETURN
      RMKSC=RMSCF
#if defined(CERNLIB_PHIGS)
      CALL PSMKSC(RMKSC)
#endif
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IMKSCO)
#endif
      END
