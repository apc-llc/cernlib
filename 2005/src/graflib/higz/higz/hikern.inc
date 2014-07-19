*
* $Id: hikern.inc,v 1.3 1998/01/28 14:34:07 couet Exp $
*
* $Log: hikern.inc,v $
* Revision 1.3  1998/01/28 14:34:07  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:57:09  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:17  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HIKERN_INC
#define CERNLIB_HIGZ_HIKERN_INC
*
*   COMMON BLOCK for the GL interface
*
* hikern.inc
*
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_MSDOS))
#if defined(CERNLIB_GL)
#if defined(CERNLIB_SGI)
#include "higz/glsgi.inc"
#endif
#if defined(CERNLIB_IBMRT)
#include "higz/glibm.inc"
#endif
      PARAMETER (IBUFSI=1000)
      COMMON /HIKERN/ WKSC(10),WINSIZ(4),IPOS(2,IBUFSI),IXX,IYY,ICURWK
     +,LOINS,ICURCI(10),IOFCOL,IXLOCO,IYLOCO,LUNWIN
      LOGICAL LOINS
      INTEGER WINSIZ,WINID
      INTEGER*2 IVAL
      COMMON /HIKERC/ FILEN
      CHARACTER*80 FILEN

#endif
#endif
*
*   COMMON BLOCK for the X11 interface
*
* hikern.inc
*
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_MSDOS))
#if (defined(CERNLIB_X11))&&(!defined(CERNLIB_WINNT))
      PARAMETER (IBUFSI=1000)
      COMMON /HIKERN/ WKSC(10),ICURWK,WINSIZ(4)
     +,IPOS(2,IBUFSI),LUNWIN,THX11,IFPX11,RMAGN
      INTEGER WINSIZ
      INTEGER*2 IPOS
      COMMON /HIKERC/ CHHOST,FILEN,FX11
      CHARACTER*80 CHHOST,FILEN,FX11

#endif
#endif
*
*   COMMON BLOCK for the Windows NT interface
*
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_MSDOS))
#if defined(CERNLIB_WINNT)
      PARAMETER (IBUFSI=1000)
      COMMON /HIKERN/ WKSC(10),ICURWK,WINSIZ(4)
     +,IPOS(2,IBUFSI),LUNWIN,THX11,IFPX11
      INTEGER WINSIZ
      COMMON /HIKERC/ CHHOST,FILEN,FX11
      CHARACTER*80 CHHOST,FILEN,FX11

#endif
#endif
*
*   COMMON BLOCK for the MacIntosh interface
*
* hikern.inc
*
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_MSDOS))
#if defined(CERNLIB_MACMPW)
      PARAMETER (IBUFSI=1000)
      COMMON /HIKERN/ WKSC(10),ICURWK,WINSIZ(4)
     +,IPOS(2,IBUFSI),LUNWIN
      INTEGER WINSIZ
      COMMON /HIKERC/ FILEN
      CHARACTER*80 FILEN

#endif
#endif
* COMMON BLOCK for the NDP_PC interface
*
* hikern.inc
*
#if defined(CERNLIB_MSDOS)
#if defined(CERNLIB_NDP)
#include "higz/grex.inc"
      PARAMETER (IBUFSI=1000)
      PARAMETER (ISURF= 18)
      PARAMETER (IRIS = 17)

#include "higz/golden.inc"

       COMMON /HIKERN/ WKSC(10),WINSIZ(4),IPOS(IBUFSI,2),IXX,IYY,ICURWK
     +,LOINS,ICURCI(10),IOFCOL,IXLOCO,IYLOCO,IWKNB,LUNMET(10)
     *, MAX_COLOR(10), IER, DASH_PATTERN(16), video_system(4)
     *, PAL, ISTEP, MAG_X, MAG_Y, MOUSE, IXSAVE(10), IYSAVE(10)
       LOGICAL LOINS, MOUSE
      INTEGER WINSIZ,WINID,video_system
      CHARACTER*4 DASH_PATTERN
      CHARACTER PAL(20)
C---      INTEGER STRWID
#endif
#endif
* COMMON BLOCK for the NDP_PC interface
#if defined(CERNLIB_MSDOS)
#if !defined(CERNLIB_NDP)
#include "higz/grex.inc"
      PARAMETER (IBUFSI=1000)
      PARAMETER (ISURF= 18)
      PARAMETER (IRIS = 17)

#include "higz/golden.inc"

       COMMON /HIKERN/ WKSC(10),WINSIZ(4),IPOS(2,IBUFSI),IXX,IYY,ICURWK
     +,LOINS,ICURCI(10),IOFCOL,IXLOCO,IYLOCO,IWKNB,LUNMET(10)
     *, MAX_COLOR(10), IER, DASH_PATTERN(16), video_system(4)
     *, PAL, ISTEP, MAG_X, MAG_Y, MOUSE, IXSAVE(10), IYSAVE(10)
       LOGICAL LOINS, MOUSE
      INTEGER WINSIZ,WINID,video_system
      CHARACTER*4 DASH_PATTERN
      CHARACTER PAL(20)
C---      INTEGER STRWID
#endif
#endif

#endif
