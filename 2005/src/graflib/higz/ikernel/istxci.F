*
* $Id: istxci.F,v 1.3 1998/01/28 14:34:58 couet Exp $
*
* $Log: istxci.F,v $
* Revision 1.3  1998/01/28 14:34:58  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:59:01  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:58  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/07 12/01/94  10.06.24  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISTXCI(ICOLI)
      ENTRY      GSTXCI(ICOLI)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
      IF(ICOLI.EQ.ITXCI)RETURN
      ITXCI=ICOLI
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ITXCCO)
#endif
      IF(GFLAG)THEN
#if defined(CERNLIB_PHIGS)
         CALL PSTXCI(ITXCI)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
         DO 10 IWKNB=1,INOPWK
            IF(NODRFL(IWKNB))GOTO 10
            CALL IGSG(IWKNB)
#endif
#if defined(CERNLIB_FALCO)
            IF(IWTYL(IWKNB).EQ.7878.OR.IWTYL(IWKNB).EQ.7879)GOTO 10
#endif
#if defined(CERNLIB_GL)
            CALL COLOR(ITXCI+IOFCOL)
            ICURCI(IWKNB)=ITXCI
#endif
#if defined(CERNLIB_X11)
            CALL IXSETTC(ITXCI)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
   10    CONTINUE
#endif
      ENDIF
      END
