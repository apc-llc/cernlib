*
* $Id: islwsc.F,v 1.3 1998/01/28 14:34:54 couet Exp $
*
* $Log: islwsc.F,v $
* Revision 1.3  1998/01/28 14:34:54  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:58:58  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:57  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/07 12/01/94  10.04.13  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISLWSC(WIDTH)
      ENTRY      GSLWSC(WIDTH)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
      IF(WIDTH.EQ.RLWSC)RETURN
      RLWSC=WIDTH
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ILWSCO)
#endif
      IF(GFLAG)THEN
#if defined(CERNLIB_PHIGS)
         CALL PSLWSC(RLWSC)
#endif
#if !defined(CERNLIB_BATCH)
         DO 10 IWKNB=1,INOPWK
            IF(NODRFL(IWKNB))GOTO 10
            CALL IGSG(IWKNB)
#endif
#if defined(CERNLIB_FALCO)
            IF(IWTYL(IWKNB).EQ.7878.OR.IWTYL(IWKNB).EQ.7879)GOTO 10
#endif
#if defined(CERNLIB_GL)
            CALL LINEWI(INT(RLWSC))
#endif
#if defined(CERNLIB_X11)
            CALL IXSETLN(MAX(INT(RLWSC/2.2),1))
#endif
#if defined(CERNLIB_MACMPW)
            CALL IMSETLN(INT(RLWSC))
#endif
#if !defined(CERNLIB_BATCH)
   10    CONTINUE
#endif
      ENDIF
      END
