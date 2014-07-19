*
* $Id: dzeqit.F,v 1.1.1.1 1996/03/04 16:13:20 mclareni Exp $
*
* $Log: dzeqit.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:20  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEQIT
#include "dzeditkeep.inc"
      CALL KUEXEC('EXIT')
      IQUITF=1
      END
*********************************************************************
