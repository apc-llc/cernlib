*
* $Id: ischh.F,v 1.2 1998/01/28 14:34:48 couet Exp $
*
* $Log: ischh.F,v $
* Revision 1.2  1998/01/28 14:34:48  couet
* - GGDM driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:56  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/10 22/06/93  17.23.31  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISCHH(CHH)
      ENTRY      GSCHH(CHH)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#if defined(CERNLIB_MSDOS)
#include "higz/hikern.inc"
#endif
#if defined(CERNLIB_MSDOS)
#include "higz/hiwcdc.inc"
#endif
      IF(CHH.EQ.RCHH)RETURN
      RCHH=CHH
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ICHHCO)
#endif
      IF(GFLAG)THEN
#if defined(CERNLIB_FALCO)
         IF(IWTYPE.EQ.7878.OR.IWTYPE.EQ.7879)RETURN
#endif
#if defined(CERNLIB_PHIGS)
         CALL PSCHH(CHH)
#endif
#if defined(CERNLIB_MSDOS)
         ISAVE=IWKNB
         IWKNB=IGIWIN(1)
         IF(IWKNB.NE.0)THEN
            MAG_Y=IABS(IYWCDC(RCHH)-IYWCDC(0.0))/8
            IF(MAG_Y.EQ.0)MAG_Y=1
            MAG_X=MAG_Y
            IER=MAGNIFY_TEXT(MAG_X,MAG_Y)
         ENDIF
         IWKNB = ISAVE
#endif
      ENDIF
      END
