*
* $Id: istxal.F,v 1.1.1.1 1996/02/14 13:10:58 mclareni Exp $
*
* $Log: istxal.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:58  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/03 05/09/95  15.03.12  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISTXAL(JTXALH,JTXALV)
      ENTRY      GSTXAL(JTXALH,JTXALV)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
      IF((JTXALH.EQ.ITXALH).AND.(JTXALV.EQ.ITXALV))RETURN
      ITXALH=JTXALH
      ITXALV=JTXALV
      ITXALG=10*JTXALH+JTXALV
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ITXACO)
#endif
      IF(GFLAG)THEN
#if defined(CERNLIB_FALCO)
         IF(IWTYPE.EQ.7878.OR.IWTYPE.EQ.7879)RETURN
#endif
#if defined(CERNLIB_X11)
         CALL IXSETTA(ITXALH,ITXALV)
#endif
      ENDIF
      END
