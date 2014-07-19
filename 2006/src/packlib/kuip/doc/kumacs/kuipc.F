*
* $Id: kuipc.F,v 1.1.1.1 1996/03/08 15:33:02 mclareni Exp $
*
* $Log: kuipc.F,v $
* Revision 1.1.1.1  1996/03/08 15:33:02  mclareni
* Kuip
*
*
#include "sys/CERNLIB_machine.h"
#include "_kuip/pilot.h"
*CMZ :  1.68/00 02/03/92  13.55.43  by  Fons Rademakers
*-- Author :

*****************************************************************
*                                                               *
            macro  kuipc  name=x  os=x  vers=x  dev=x
*                                                               *
*****************************************************************

mess 'Compiling kuipc....'

sel kuipcpat

if [name] = apollo goto apollo
if [os]   = unix   goto unix
if [os]   = vm     goto vm
if [os]   = newlib goto newlib
if [os]   = vms    goto vms
   goto error

apollo:
   set kuipc.ftn -f
   cfor /kuipc
   if [dev] <> x goto devapo
      shell bind -sparse_vm -b kuipc kuipc.bin _
            /cern/[vers]/lib/libpacklib.a _
            /cern/[vers]/lib/libkernlib.a
      goto end
   devapo:
      shell bind -sparse_vm -b kuipc kuipc.bin _
            kuip.lib $HOME/kern/kernlib.lib
      goto end

unix:
   set kuipc.f -f
   cfor /kuipc
   if [dev] <> x goto devunix
      shell f77 -o kuipc kuipc.o _
            /cern/[vers]/lib/libpacklib.a _
            /cern/[vers]/lib/libkernlib.a
      goto end
   devunix:
      shell f77 -o kuipc kuipc.o _
            kuip.a $HOME/kern/kernlib.a
      goto end

vm:
   set kuipc.fortran.a -f
   cfor /kuipc
   * cannot bind from CMZ
   goto end

newlib:
   mess '*** set directives for kuipc correctly ***'
*   set kuipc.fortran.a -f
*   cfor /kuipc
   goto error

vms:
   set kuipc.for -f
   cfor /kuipc
   if [dev] <> x goto devvms
      shell link/exe=kuipc kuipc.obj, _
            'cern:['//[vers]//'.lib]packlib/lib,kernlib/lib'
      goto end
   devvms:
      shell link/debug/exe=kuipc kuipc.obj,kuip.olb, _
            'cern:['//[vers]//'.lib]kernlib/lib
      goto end

error:
mess *** Error in load: name = [name], os = [os] ***

end:
mess 'Compiling kuipc....done'

return

