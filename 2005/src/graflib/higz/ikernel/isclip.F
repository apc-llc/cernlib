*
* $Id: isclip.F,v 1.3 1998/01/28 14:34:50 couet Exp $
*
* $Log: isclip.F,v $
* Revision 1.3  1998/01/28 14:34:50  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:58:52  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:56  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/01 10/05/94  10.52.06  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISCLIP(ICLSW)
      ENTRY      GSCLIP(ICLSW)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_KERNEL)
#include "higz/hiwcdc.inc"
#endif
      ICLIP=ICLSW
      IF(GFLAG)THEN
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
         DO 10 IWKNB=1,INOPWK
            IF(NODRFL(IWKNB))GOTO 10
            CALL IGSG(IWKNB)
#endif
#if defined(CERNLIB_FALCO)
            IF(IWTYL(IWKNB).EQ.7878.OR.IWTYL(IWKNB).EQ.7879)GOTO 10
#endif
#if defined(CERNLIB_GL)
            IF(ICLIP.NE.0)THEN
               WINSIZ(1)=IXWCDC(RWXMIN)
               WINSIZ(2)=IXWCDC(RWXMAX)
               WINSIZ(3)=IYWCDC(RWYMIN)
               WINSIZ(4)=IYWCDC(RWYMAX)
            ELSE
               WINSIZ(1)=0
               WINSIZ(2)=INT(XWKSIZ(IWKNB))
               WINSIZ(3)=0
               WINSIZ(4)=INT(YWKSIZ(IWKNB))
            ENDIF
            CALL SCRMAS(WINSIZ(1),WINSIZ(2),WINSIZ(3),WINSIZ(4))
#endif
#if defined(CERNLIB_X11)
            IF(ICLIP.NE.0)THEN
               IX=IXWCDC(RWXMIN)
               IY=IYWCDC(RWYMAX)
               IW=IXWCDC(RWXMAX)-IXWCDC(RWXMIN)
               IH=IYWCDC(RWYMIN)-IYWCDC(RWYMAX)
               CALL IXCLIP(IWINID(IWKNB),IX,IY,IW,IH)
            ELSE
               CALL IXNOCLI(IWINID(IWKNB))
            ENDIF
#endif
#if defined(CERNLIB_MACMPW)
            IF(ICLIP.NE.0)THEN
               IX=IXWCDC(RWXMIN)
               IY=IYWCDC(RWYMAX)
               IW=IXWCDC(RWXMAX)-IXWCDC(RWXMIN)
               IH=IYWCDC(RWYMIN)-IYWCDC(RWYMAX)
               CALL IMCLIP(IX,IY,IW,IH)
            ELSE
               CALL IMNOCLI
            ENDIF
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
   10    CONTINUE
#endif
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZSCLI(ICLSW)
#endif
      END
