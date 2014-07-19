*
* $Id: istxfp.F,v 1.2 1996/09/25 14:59:02 couet Exp $
*
* $Log: istxfp.F,v $
* Revision 1.2  1996/09/25 14:59:02  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:58  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/09 26/01/94  09.10.03  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISTXFP(JFONT,JPREC)
      ENTRY      GSTXFP(JFONT,JPREC)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*
      IF((JFONT.EQ.IFONT).AND.(JPREC.EQ.IPREC))RETURN
      IFONT=JFONT
      IPREC=JPREC
#if defined(CERNLIB_MACMPW)
      IF(.NOT.((IFONT.EQ.0.AND.IPREC.EQ.2).OR.
     +  (IFONT.LT.0.AND.IFONT.GT.-25.AND.IPREC.EQ.1)))THEN
        IFONT=0
        IPREC=2
      ENDIF
#endif
      IFTPR=10*JFONT+ISIGN(1,JFONT)*JPREC
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ITXFCO)
#endif
      IF(IPREC.EQ.2)RETURN
      IF(GFLAG)THEN
#if defined(CERNLIB_FALCO)
         IF(IWTYPE.EQ.7878.OR.IWTYPE.EQ.7879)THEN
            IF(IFONT.LT.0.AND.IFONT.GT.-25.AND.IPREC.EQ.1)RETURN
            IFONT=0
            IPREC=2
            RETURN
         ENDIF
#endif
#if defined(CERNLIB_PHIGS)
         CALL PSTXPR(JPREC)
         CALL PSTXFN(JFONT)
#endif
#if !defined(CERNLIB_BATCH)
         DO 10 IWKNB=1,INOPWK
            IF(NODRFL(IWKNB))GOTO 10
            CALL IGSG(IWKNB)
#endif
#if !defined(CERNLIB_BATCH)
   10    CONTINUE
#endif
      ENDIF
      END
