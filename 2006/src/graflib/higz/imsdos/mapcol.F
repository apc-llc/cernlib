*
* $Id: mapcol.F,v 1.1.1.1 1996/02/14 13:11:03 mclareni Exp $
*
* $Log: mapcol.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:03  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.16/07 26/11/92  11.37.03  by  O.Couet
*-- Author : V.Fine
      SUBROUTINE MAPCOL(ICI,RED,GREEN,BLUE)
#include "higz/hikern.inc"
#include "higz/hiatt.inc"
      PARAMETER (IVGA = 18)

      if (ICI .gt. INBCOL) return
       IRED  = MIN(NINT(RED*3),3)
       IGREEN= MIN(NINT(GREEN*3),3)
       IBLUE = MIN(NINT(BLUE*3),3)
       IIIC = ISHFT(IAND(IRED,2),4)+ISHFT(IAND(IRED,1),2)
       IIIC2= ISHFT(IAND(IGREEN,2),3)+ISHFT(IAND(IGREEN,1),1)
       IIIC = IOR(IIIC,IIIC2)
       IIIC2= ISHFT(IAND(IBLUE,2),2)+JBIT(IBLUE,1)
       IIIC = IOR(IIIC,IIIC2)
      IF (IWTYL(Icurwk) .LT. IVGA) THEN
        CALL DEFINE_COLOR(ICI,IIIC)
        PAL(ICI+1) = CHAR(IIIC)
      ELSE
**      IER    = SET_PALETTE(PAL)
        IRED   =  NINT(RED*63)
        IGREEN =  NINT(GREEN*63)
        IBLUE  =  NINT(BLUE*63)
        CALL DEFINE_COLOR(ICI,ICI+IOFCOL)
        PAL(ICI+1) = CHAR(ICI+IOFCOL)
        CALL VGA_PALETTE(ICI+IOFCOL,IRED,IGREEN,IBLUE)
      END IF
      END
#endif
