*
* $Id: adwrtk.F,v 1.1.1.1 1996/02/15 17:50:06 mclareni Exp $
*
* $Log: adwrtk.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:06  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
@PROCESS AUTODBL(NONE)
      SUBROUTINE ADWRTK(LUN,IA,ND,ISW)
      INTEGER IA(2*ND)
      INTEGER ISW
*
      WRITE(LUN,IOSTAT=ISW) (IA(J),J=1,2*ND)
      END
