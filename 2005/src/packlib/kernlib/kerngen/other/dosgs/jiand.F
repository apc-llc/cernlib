*
* $Id: jiand.F,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
*
* $Log: jiand.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
        function jiand(ia,ib)
#include "kerngen/q_andor.inc"
        jiand = iand(ia,ib)
        end
