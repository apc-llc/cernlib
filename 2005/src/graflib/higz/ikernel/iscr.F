*
* $Id: iscr.F,v 1.2 1996/09/25 14:58:53 couet Exp $
*
* $Log: iscr.F,v $
* Revision 1.2  1996/09/25 14:58:53  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:56  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/07 12/01/94  09.48.29  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISCR(IWKID,ICI,CR,CG,CB)
      ENTRY      GSCR(IWKID,ICI,CR,CG,CB)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/hilut.inc"
#if defined(CERNLIB_X11)||defined(CERNLIB_FALCO)
#include "higz/hiatt.inc"
#endif
*
      I=MIN(NBCLUT-1,ABS(ICI))+1
      REDLUT(I)=CR
      GRNLUT(I)=CG
      BLULUT(I)=CB
*
      IF(GFLAG)THEN
         J=IGIWTY(IWKID)
         IF(J.LE.0)RETURN
#if defined(CERNLIB_FALCO)
         IF(J.EQ.7878.OR.J.EQ.7879)RETURN
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
         CALL MAPCOL(ICI+IOFCOL,INT(255.*CR),INT(255.*CG),INT(255.*CB))
#endif
#if defined(CERNLIB_MSDOS)
         CALL MAPCOL(ICI+IOFCOL,CR,CG,CB)
#endif
#if defined(CERNLIB_X11)
         CALL IXSETCO(ICI,CR,CG,CB)
         IF(IPLCI.EQ.ICI)CALL IXSETLC(ICI)
         IF(IFACI.EQ.ICI)CALL IXSETFC(ICI)
         IF(IPMCI.EQ.ICI)CALL IXSETMC(ICI)
         IF(ITXCI.EQ.ICI)CALL IXSETTC(ICI)
#endif
#if defined(CERNLIB_MACMPW)
         CALL IMSETCO(ICI,CR,CG,CB)
#endif
#if defined(CERNLIB_PHIGS)
         CALL PSCR(IWKID,ICI,CR,CG,CB)
#endif
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZSCOR(IWKID,ICI,CR,CG,CB)
#endif
      END
