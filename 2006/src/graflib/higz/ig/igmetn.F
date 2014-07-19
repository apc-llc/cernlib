*
* $Id: igmetn.F,v 1.1.1.1 1996/02/14 13:10:36 mclareni Exp $
*
* $Log: igmetn.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:36  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/01 04/05/93  13.26.22  by  O.Couet
*-- Author :    O.Couet   04/05/93
      SUBROUTINE IGMETN(CHNAME)
*.===========>
*.
*.  Set the metafile name
*.
*..==========>
#include "higz/himeta.inc"
      CHARACTER*(*) CHNAME
      CHMETA = CHNAME
      END
