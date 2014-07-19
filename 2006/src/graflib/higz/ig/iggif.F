*
* $Id: iggif.F,v 1.4 2000/08/29 14:01:43 couet Exp $
*
* $Log: iggif.F,v $
* Revision 1.4  2000/08/29 14:01:43  couet
* - An error message in printed when this routine is not used in the X11
*   version of HIGZ.
*
* Revision 1.3  1998/11/16 11:11:24  couet
* - ixdogif can scale pictures
*
* Revision 1.2  1996/02/19 16:17:05  couet
* The include file containing the inline functions IXWCDC and IYWCDC was
* missing in the IGGIF routine.
*
* Revision 1.1.1.1  1996/02/14 13:10:41  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 10/01/96  13.29.50  by  O.Couet
*-- Author :    O.Couet   07/12/95
      SUBROUTINE IGGIF(IWKID,X,Y,CHNAME,CHOPT)
*.==============>
*.
*.   Generate/load a GIF file with/in the current X11 window.
*.
*. _Input parameters:
*.
*.  CHARACTER CHNAME : GIF file name.
*.
*.============> (O.Couet)
#if defined(CERNLIB_GIF)
#include "higz/hiatt.inc"
#endif
      CHARACTER*(*) CHNAME,CHOPT
*.______________________________________
*
#if defined(CERNLIB_GIF)
#include "higz/hiwcdc.inc"
#endif
#if defined(CERNLIB_GIF)
      ILEN = LENOCC(CHNAME)
      CALL IXSAVWI
      IW    = IWINID(IGIWIN(IWKID))
      CALL IXSELWI(IW)
      IWKNB = IGIWIN(IWKID)
*
      IF (INDEX(CHOPT,'M').NE.0) THEN
         IX = INT(X)
         IY = INT(Y)
         CALL IXDOGIF(IX,IY,ILEN,CHNAME(1:ILEN))
      ENDIF
*
      IF (INDEX(CHOPT,'L').NE.0) THEN
         IX = IXWCDC(X)
         IY = IYWCDC(Y)
         CALL IXLDGIF(IX,IY,ILEN,CHNAME(1:ILEN))
      ENDIF
*
      CALL IXSETWI
#endif
#if !defined(CERNLIB_GIF)
      CALL IGERR('Only HIGZ/X11 can do GIF files','IGGIF')
#endif
*
      END
