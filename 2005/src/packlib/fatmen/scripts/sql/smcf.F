*
* $Id: smcf.F,v 1.1.1.1 1996/03/07 15:17:51 mclareni Exp $
*
* $Log: smcf.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:51  mclareni
* Fatmen
*
*
#include "sys/CERNLIB_machine.h"
#include "_fatmen/pilot.h"
select count(vsn) from fatmen.volumes_&1 where vsn like 'I%';
