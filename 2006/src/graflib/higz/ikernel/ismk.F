*
* $Id: ismk.F,v 1.2 1998/01/28 14:34:55 couet Exp $
*
* $Log: ismk.F,v $
* Revision 1.2  1998/01/28 14:34:55  couet
* - GGDM driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:57  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/01 05/05/93  14.09.03  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISMK(MTYPE)
      ENTRY      GSMK(MTYPE)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
      IF(MTYPE.EQ.IMK)RETURN
      IMK=MTYPE
#if !defined(CERNLIB_X11)
      IF(IMK.EQ.3)IMK=31
#endif
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IMKCO)
#endif
#if defined(CERNLIB_PHIGS)
      CALL PSMK(MTYPE)
#endif
      END
