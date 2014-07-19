*
* $Id: igsg.F,v 1.2 1996/09/25 14:58:45 couet Exp $
*
* $Log: igsg.F,v $
* Revision 1.2  1996/09/25 14:58:45  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/06 07/01/94  15.19.30  by  O.Couet
*-- Author :    O.Couet   06/02/90
      SUBROUTINE IGSG(IWK)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#if defined(CERNLIB_MSDOS)
#include "higz/himeta.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_MSDOS)
      PARAMETER (IVGA=18)
#endif
      IF(ICURWK.NE.IWK)THEN
#if defined(CERNLIB_FALCO)
         IF(IGIWTY(IWK).EQ.7878.OR.IGIWTY(IWK).EQ.7879)GOTO 10
#endif
#if defined(CERNLIB_GL)
         CALL WINSET(IWINID(IWK))
#endif
#if defined(CERNLIB_X11)
         CALL IXSELWI(IWINID(IWK))
#endif
         ICURWK=IWK
      ENDIF
   10 IF(.NOT.GRFLAG)THEN
#if defined(CERNLIB_MSDOS)
         IF(ILOPWK(IWK).NE.IDMETA)THEN
            I = GET_VIDEO_MODE(LRX,LCY)
            IF (I .GT. 0 .and. I .le. 7) THEN
              IF (IWTYL(IWK) .LE. IVGA+1) THEN
                 IER = GRAPHICS_MODE(IWTYL(IWK))
              ELSE
                 IER = SUPER_VGA(IWTYL(IWK), 800, 600)
              END IF
            END IF
            IF (IWTYL(IWK) .LT. IVGA) THEN
               IER = SET_PALETTE(PAL)
            ELSE
               CALL MAPCOL(0,1.,1.,1.)
               CALL MAPCOL(1,0.,0.,0.)
               CALL MAPCOL(2,1.,0.,0.)
               CALL MAPCOL(3,0.,1.,0.)
               CALL MAPCOL(4,0.,0.,1.)
               CALL MAPCOL(5,1.,1.,0.)
               CALL MAPCOL(6,1.,0.,1.)
               CALL MAPCOL(7,0.,0.,0.)
            END IF
         ENDIF
#endif
         GRFLAG=.TRUE.
#if defined(CERNLIB_FALCO)
         IF(IGIWTY(IWK).EQ.7879)THEN
            CALL IFPUT(27)
            CALL IFPUT(IGASCI('['))
            CALL IFPUT(IGASCI('?'))
            CALL IFPUT(IGASCI('3'))
            CALL IFPUT(IGASCI('8'))
            CALL IFPUT(IGASCI('h'))
            CALL IFPUT(0)
         ENDIF
#endif
      ENDIF
      END
