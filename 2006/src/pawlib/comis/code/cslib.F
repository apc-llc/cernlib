*
* $Id: cslib.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: cslib.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.13/00 27/03/92  15.51.08  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSLIB(LIBNAM)
***********************************************
*                                             *
*   OPEN THE TEXT LIBRARY LIBNAM FOR READ.    *
*   DEFAULT FILETYPE IS '.TLB'                *
*                                             *
*   ISTLIB = 1, IF OPERATION IS SUCCESSEFUL.  *
*                                             *
***********************************************
      CHARACTER*(*) LIBNAM
#include "comis/cslun.inc"
      COMMON /CSLBRW/ LIBIND,LIBST,LEOF,LIBRD,LIBTXT
*
*   FIRST INITIALIZE THE LIBRARY INDEX
*
      ISTLIB=0
#if defined(CERNLIB_VAX)
      LIBST=LBR$INI_CONTROL(LIBIND,LIBRD,LIBTXT)
      IF(.NOT.LIBST) RETURN
*
*   OPEN THE LIBRARY
*
      LIBST=LBR$OPEN(LIBIND,LIBNAM,,'.TLB')
      IF(.NOT.LIBST) RETURN
      ISTLIB=1
#endif
      END
