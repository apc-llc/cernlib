*
* $Id: igterm.F,v 1.3 1996/04/16 08:12:23 couet Exp $
*
* $Log: igterm.F,v $
* Revision 1.3  1996/04/16 08:12:23  couet
* - rearange the #ifdef
* - test on IZBUF was missing (the zbuffer is reactivated)
*
* Revision 1.2  1996/04/16 07:38:18  couet
* The ZBUFER is temporarly deactivated for PS. It generated some
* 'gr' out of context.
*
* Revision 1.1.1.1  1996/02/14 13:10:40  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/06 04/12/95  15.30.10  by  O.Couet
*-- Author :    O.Couet   26/06/89
      SUBROUTINE IGTERM
#include "higz/hiflag.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if (defined(CERNLIB_ZBUF))&&(defined(CERNLIB_PSCRIPT))
#include "higz/hipost.inc"
#include "higz/hilut.inc"
#include "higz/hiatt.inc"
#include "higz/hiwcps.inc"
#endif
#if defined(CERNLIB_MAIL)
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3)') 559
         CALL IMWRIT(4)
         RETURN
      ENDIF
#endif
#if defined(CERNLIB_GKS)
      CALL IUWK(0,0)
#endif
#if !defined(CERNLIB_GKS)
      CALL IUWK(0,1)
#endif
      CALL IGSA(0)
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_ZBUF))&&(defined(CERNLIB_PSCRIPT))
      IF (PFLAG.AND.IZBUF.NE.0) THEN
         CALL IGNG3D(W3X1,W3X2,W3Y1,W3Y2,W3Z1,W3Z2
     +,              PHI,THETA,PSI
     +,              V3X1,V3X2,V3Y1,V3Y2)
         IXPS1 = IXWCPS(V3X1)
         IXPS2 = IXWCPS(V3X2)
         IYPS1 = IYWCPS(V3Y1)
         IYPS2 = IYWCPS(V3Y2)
         CALL IPPSTR('@')
         CALL IHZPS(0,0,IXPS1,IYPS1,IXPS2,IYPS2,
     +              INBCOL,REDLUT,GRNLUT,BLULUT)
         CALL IPPFST('gr')
      ENDIF
#endif
      END
