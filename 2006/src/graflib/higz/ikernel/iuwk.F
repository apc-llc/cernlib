*
* $Id: iuwk.F,v 1.1.1.1 1996/02/14 13:10:59 mclareni Exp $
*
* $Log: iuwk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:59  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/06 04/12/95  15.28.25  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IUWK(IWKID,IRFLG)
      ENTRY      GUWK(IWKID,IRFLG)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_ZBUF)
#include "higz/hiwcdc.inc"
#endif
#if defined(CERNLIB_PHIGS)
      CALL PXCLOSE_PIPE
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_ZBUF))
      CALL IGNG3D(W3X1,W3X2,W3Y1,W3Y2,W3Z1,W3Z2
     +,           PHI,THETA,PSI
     +,           V3X1,V3X2,V3Y1,V3Y2)
#endif
      IF(IWKID.NE.0)THEN
         ITY=IGIWTY(IWKID)
         IF(ITY.GT.0.AND.GRFLAG)THEN
#if defined(CERNLIB_FALCO)
            IF(ITY.EQ.7878.OR.ITY.EQ.7879)RETURN
#endif
#if (defined(CERNLIB_GL))&&(defined(CERNLIB_SGI))
            CALL GFLUSH
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_ZBUF))
            IF (IZBUF.NE.0) THEN
               IWKNB=1
               IZX = IXWCDC(V3X1)
               IZY = IYWCDC(V3Y2)
               CALL IHZTOX(0,IZX,IZY)
            ENDIF
#endif
#if defined(CERNLIB_X11)
            CALL IXSELWI(IWINID(IGIWIN(IWKID)))
            CALL IXUPDWI(1)
            CALL IKUWK(IWKID)
#endif
#if defined(CERNLIB_MACMPW)
            CALL IMUPDWI
#endif
#if defined(CERNLIB_PHIGS)
            CALL PXOPEN_PIPE(IWKID)
#endif
#if defined(CERNLIB_MSDOS)
            CALL IMFUWK(ITY)
#endif
         ENDIF
      ELSE
         DO 10 I=1,INOPWK
            ITY=IGIWTY(ILOPWK(I))
            IF(ITY.GT.0.AND.ACWKFL(I))THEN
#if defined(CERNLIB_FALCO)
               IF(ITY.EQ.7878.OR.ITY.EQ.7879)GOTO 10
#endif
#if (defined(CERNLIB_GL))&&(defined(CERNLIB_SGI))
               CALL GFLUSH
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_ZBUF))
               IF (IZBUF.NE.0) THEN
                  IWKNB=I
                  IZX = IXWCDC(V3X1)
                  IZY = IYWCDC(V3Y2)
                  CALL IHZTOX(0,IZX,IZY)
               ENDIF
#endif
#if defined(CERNLIB_X11)
               CALL IXSELWI(IWINID(I))
               CALL IXUPDWI(1)
               CALL IKUWK(ILOPWK(I))
#endif
#if defined(CERNLIB_MACMPW)
               CALL IMUPDWI
#endif
#if defined(CERNLIB_PHIGS)
               CALL PXOPEN_PIPE(ILOPWK(I))
#endif
#if defined(CERNLIB_MSDOS)
               CALL IMFUWK(ITY)
#endif
            ENDIF
   10    CONTINUE
      ENDIF
      END
