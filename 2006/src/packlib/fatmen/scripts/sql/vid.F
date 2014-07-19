*
* $Id: vid.F,v 1.1.1.1 1996/03/07 15:17:51 mclareni Exp $
*
* $Log: vid.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:51  mclareni
* Fatmen
*
*
#include "sys/CERNLIB_machine.h"
#include "_fatmen/pilot.h"
column gname format a80
select gname "FATMEN Generic Name " from fatmen.volumes_&2,fatmen.fxv_&2,
                   fatmen.files_&2,fatmen.gnames_&2
   where fatmen.gnames_&2..gn#=fatmen.files_&2..gn# and
         fatmen.files_&2..file#=fatmen.fxv_&2..file# and
         fatmen.fxv_&2..vol#=fatmen.volumes_&2..vol# and
         vsn='&1' and active='Y' ;
