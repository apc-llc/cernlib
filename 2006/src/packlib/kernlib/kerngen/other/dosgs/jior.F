*
* $Id: jior.F,v 1.1.1.1 1996/02/15 17:50:05 mclareni Exp $
*
* $Log: jior.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:05  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
        function jior(ia,ib)
#include "kerngen/q_andor.inc"
        jior = ior(ia,ib)
        end
