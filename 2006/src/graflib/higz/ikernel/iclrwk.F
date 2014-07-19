*
* $Id: iclrwk.F,v 1.3 1998/01/28 14:34:40 couet Exp $
*
* $Log: iclrwk.F,v $
* Revision 1.3  1998/01/28 14:34:40  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:58:41  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if !defined(CERNLIB_MSDOS)
*CMZ :  1.23/06 20/11/95  17.15.55  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ICLRWK(IWKID,ICOFL)
      ENTRY      GCLRWK(IWKID,ICOFL)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      DIMENSION RVAL(2)
*
      IF(GFLAG)THEN
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)||defined(CERNLIB_PHIGS)
         IF(IWKID.EQ.0)THEN
            DO 10 I=1,INOPWK
               IF(NODRFL(I))GOTO 10
               CALL IGQWK(ILOPWK(I),'MXDS',RVAL)
               IF((REDIT.NE.0).AND.(REDIT.NE.1.))THEN
                  CALL IGCLES
               ELSE
                  CALL IGSG(I)
#endif
#if defined(CERNLIB_FALCO)
                  IF(IWTYL(I).EQ.7878.OR.IWTYL(I).EQ.7879)THEN
                     CALL IFCLR(IWTYL(I))
                     GOTO 10
                  ENDIF
#endif
#if defined(CERNLIB_GL)
                  CALL WINPOP
                  CALL SETPAT(0)
                  CALL COLOR(IOFCOL)
                  ICURCI(I)=0
                  CALL CLEAR
                  IF(IFAIS.GT.1)CALL SETPAT(IFASI)
#endif
#if defined(CERNLIB_X11)
                  CALL IXCLRWI
#endif
#if defined(CERNLIB_ZBUF)
                  IF (IZBUF.NE.0) CALL IHZCLE(0)
#endif
#if defined(CERNLIB_MACMPW)
                  CALL IMCLRWI
#endif
#if defined(CERNLIB_PHIGS)
                  CALL PXCLEAR_WS(ILOPWK(I))
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)||defined(CERNLIB_PHIGS)
               ENDIF
   10       CONTINUE
         ELSE
            IWT=IGIWTY(IWKID)
            IF(IWT.LE.0)GOTO 999
            CALL IGQWK(IWKID,'MXDS',RVAL)
            IF((REDIT.NE.0).AND.(REDIT.NE.1.))THEN
               CALL IGCLES
            ELSE
               I=IGIWIN(IWKID)
               CALL IGSG(I)
#endif
#if defined(CERNLIB_FALCO)
               IF(IWT.EQ.7878.OR.IWT.EQ.7879)THEN
                  CALL IFCLR(IWT)
                  GOTO 999
               ENDIF
#endif
#if defined(CERNLIB_GL)
               CALL WINPOP
               CALL SETPAT(0)
               CALL COLOR(IOFCOL)
               ICURCI(I)=0
               CALL CLEAR
               IF(IFAIS.GT.1)CALL SETPAT(IFASI)
#endif
#if defined(CERNLIB_X11)
               CALL IXCLRWI
#endif
#if defined(CERNLIB_ZBUF)
               IF (IZBUF.NE.0) CALL IHZCLE(0)
#endif
#if defined(CERNLIB_MACMPW)
               CALL IMCLRWI
#endif
#if defined(CERNLIB_PHIGS)
               CALL PXCLEAR_WS(IWKID)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)||defined(CERNLIB_PHIGS)
            ENDIF
         ENDIF
#endif
      ENDIF
  999 CONTINUE
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZCLRD
#endif
      END
#endif
