*
* $Id: dropgrou.F,v 1.1.1.1 1996/03/07 15:17:51 mclareni Exp $
*
* $Log: dropgrou.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:51  mclareni
* Fatmen
*
*
#include "sys/CERNLIB_machine.h"
#include "_fatmen/pilot.h"
DELETE FROM GNAMES_group;
/
DELETE FROM FILES_group;
/
DELETE FROM VOLUMES_group;
/
DELETE FROM FXV_group;
/
