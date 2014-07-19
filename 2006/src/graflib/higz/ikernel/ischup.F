*
* $Id: ischup.F,v 1.2 1998/01/28 14:34:49 couet Exp $
*
* $Log: ischup.F,v $
* Revision 1.2  1998/01/28 14:34:49  couet
* - GGDM driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:56  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.12/05 05/03/91  12.06.07  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISCHUP(CHUX,CHUY)
      ENTRY      GSCHUP(CHUX,CHUY)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
      IF((CHUX.EQ.RCHUX).AND.(CHUY.EQ.RCHUY))RETURN
      RCHUX=CHUX
      RCHUY=CHUY
      IF((RCHUX.EQ.0.).AND.(RCHUY.EQ.1.))THEN
         RANGLE=0.
      ELSEIF((RCHUX.EQ.-1.).AND.(RCHUY.EQ.0.))THEN
         RANGLE=90.
      ELSEIF((RCHUX.EQ.0.).AND.(RCHUY.EQ.-1.))THEN
         RANGLE=180.
      ELSEIF((RCHUX.EQ.1.).AND.(RCHUY.EQ.0.))THEN
         RANGLE=270.
      ELSE
         RANGLE=((ACOS(RCHUX/SQRT(RCHUX**2+RCHUY**2))*180.)
     +           /3.14159)-90.
         IF(RCHUY.LT.0.)RANGLE=180.-RANGLE
         IF(ABS(RANGLE).LE.0.01)RANGLE=0.
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IANGCO)
#endif
      END
