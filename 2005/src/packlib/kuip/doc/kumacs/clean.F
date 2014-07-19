*
* $Id: clean.F,v 1.1.1.1 1996/03/08 15:33:01 mclareni Exp $
*
* $Log: clean.F,v $
* Revision 1.1.1.1  1996/03/08 15:33:01  mclareni
* Kuip
*
*
#include "sys/CERNLIB_machine.h"
#include "_kuip/pilot.h"
*CMZ :  1.68/00 02/03/92  13.55.43  by  Fons Rademakers
*-- Author :

*****************************************************************
*                                                               *
                  macro  clean  os=x  ftn=x
*                                                               *
*****************************************************************

mess 'Deleting old fortran and binary files....'

if [os] = unix goto unix
if [os] = vm   goto vm
if [os] = vms  goto vms
   goto error

unix:
   host_shell /bin/sh
*   shell /bin/rm -fr [ftn]
   shell /bin/mkdir  [ftn]
   goto end

vm:
   shell gime 20 [ftn]
*   shell erase * fortran [ftn]
*   shell erase * text [ftn]
   goto end

vms:
*   shell delete '[.'//[ftn]//']*.*;*'
*   shell delete [ftn].dir;*
   shell create/directory/protection=(o:dwre) '[.'//[ftn]//']'
   goto end

error:
mess *** Error in clean: os = [os], ftn = [ftn] ***

end:
mess 'Deleting old fortran and binary files....done'

return

