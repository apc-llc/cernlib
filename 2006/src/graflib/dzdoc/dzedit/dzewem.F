*
* $Id: dzewem.F,v 1.1.1.1 1996/03/04 16:13:22 mclareni Exp $
*
* $Log: dzewem.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:22  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEWEM(LUNSGM)
      WRITE(LUNSGM,'(A)')'</BODY>'
      WRITE(LUNSGM,'(A)')'<BACKM>'
      WRITE(LUNSGM,'(A)')'<INDEX>'
      WRITE(LUNSGM,'(A)')'</BACKM>'
      WRITE(LUNSGM,'(A)')'</GDOC>'
      END
