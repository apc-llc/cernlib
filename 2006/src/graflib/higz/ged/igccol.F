*
* $Id: igccol.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: igccol.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.11/00 05/11/90  17.05.57  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGCCOL(IVAL,CHVALI)
*.===========>
*.
*.   Convert color index in color name
*.
*..==========> (O.Couet)
      CHARACTER*(*) CHVALI
*.______________________________________
*
      IF(IVAL.EQ.2)CHVALI='Red'
      IF(IVAL.EQ.3)CHVALI='Green'
      IF(IVAL.EQ.4)CHVALI='Blue'
#if !defined(CERNLIB_DECGKS)
      IF(IVAL.EQ.0)CHVALI='Black'
      IF(IVAL.EQ.1)CHVALI='White'
      IF(IVAL.EQ.5)CHVALI='Yellow'
      IF(IVAL.EQ.6)CHVALI='Magenta'
      IF(IVAL.EQ.7)CHVALI='Cyan'
#endif
#if defined(CERNLIB_DECGKS)
      IF(IVAL.EQ.0)CHVALI='White'
      IF(IVAL.EQ.1)CHVALI='Black'
      IF(IVAL.EQ.6)CHVALI='Magenta'
      IF(IVAL.EQ.5)CHVALI='Cyan'
      IF(IVAL.EQ.7)CHVALI='Yellow'
#endif
*
      END
