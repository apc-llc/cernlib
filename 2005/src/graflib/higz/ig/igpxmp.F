*
* $Id: igpxmp.F,v 1.1.1.1 1996/02/14 13:10:37 mclareni Exp $
*
* $Log: igpxmp.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:37  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/01 25/04/94  21.28.56  by  O.Couet
*-- Author :    O.Couet   05/02/92
      SUBROUTINE IGPXMP(IPX,IX,IY,CHOPT)
*.==============>
*.
*.   X11 Pixmap manipulation.
*.
*. _Input parameters:
*.
*.  INTEGER IPX : Pixmap identifier (input or output see CHOPT).
*.  INTEGER IX,IY : Pixmap size or position (see CHOPT).
*.  CHARACTER CHOPT : Character option.
*.            CHOPT='O' Open pixmap. IPX is an output parameter
*.                      IX is the width and IY the height.
*.            CHOPT='R' Remove pixmap. IPX is an input parameter.
*.            CHOPT='D' Display pixmap. (IX,IY) is the position
*.                      where the pixpmap has to be displayed.
*.                      IPX is an input parameter.
*.            CHOPT='C' Close the current opened pixmap.
*.            CHOPT='S' Save the region of the current window at
*.                      the position (IX,IY) in preopen pixmap IPX.
*.            CHOPT='E' Erase (clear) the pixmap IPX.
*.            CHOPT='W' Write the pixmap IPX into the file 'higz.bm'.
*.                      IX is the width and IY the height. IPX is an
*.                      input parameter.
*.
*.============> (O.Couet)
#if defined(CERNLIB_X11)
      DIMENSION IOPT(7)
      EQUIVALENCE (IOPTO,IOPT(1)),(IOPTR,IOPT(2))
      EQUIVALENCE (IOPTD,IOPT(3)),(IOPTC,IOPT(4))
      EQUIVALENCE (IOPTS,IOPT(5)),(IOPTE,IOPT(6))
      EQUIVALENCE (IOPTW,IOPT(7))
      CHARACTER*(*) CHOPT
*.______________________________________
*
      CALL UOPTC(CHOPT,'ORDCSEW',IOPT)
*
      IF (IOPTO.NE.0) IPX=IXOPNPX(IX,IY)
      IF (IOPTC.NE.0) CALL IXCLPX
      IF (IOPTS.NE.0) CALL IXWIPX(IPX,IX,IY)
      IF (IOPTD.NE.0) CALL IXCPPX(IPX,IX,IY)
      IF (IOPTR.NE.0) CALL IXRMPX(IPX)
      IF (IOPTE.NE.0) CALL IXCLRPX(IPX)
      IF (IOPTW.NE.0) CALL IXWRPX(IPX,IX,IY,7,'higz.bm')
#endif
*
      END
