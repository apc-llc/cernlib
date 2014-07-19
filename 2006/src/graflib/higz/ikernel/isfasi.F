*
* $Id: isfasi.F,v 1.2 1998/01/28 14:34:52 couet Exp $
*
* $Log: isfasi.F,v $
* Revision 1.2  1998/01/28 14:34:52  couet
* - GGDM driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:57  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/01 05/05/93  14.09.03  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISFASI(ISTYL)
      ENTRY      GSFASI(ISTYL)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
      IF(ISTYL.EQ.IFASI)RETURN
      IFASI=ISTYL
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IFASCO)
#endif
      IF(GFLAG)THEN
#if defined(CERNLIB_FALCO)
         IF(IWTYPE.EQ.7878.OR.IWTYPE.EQ.7879)RETURN
#endif
#if defined(CERNLIB_GL)
         DO 10 IWKNB=1,INOPWK
            IF(NODRFL(IWKNB))GOTO 10
            CALL IGSG(IWKNB)
            IF(IFAIS.LE.1)THEN
               CALL SETPAT(0)
            ELSE
               CALL SETPAT(IFASI)
            ENDIF
   10    CONTINUE
#endif
#if defined(CERNLIB_X11)
         CALL IXSETFS(IFAIS,ABS(IFASI))
#endif
#if defined(CERNLIB_MACMPW)
         CALL IMSETFS(IFAIS,ABS(IFASI))
#endif
#if defined(CERNLIB_PHIGS)
         CALL PSISI(IFASI)
#endif
      ENDIF
      END
