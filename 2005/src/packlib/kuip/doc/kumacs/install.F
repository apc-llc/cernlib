*
* $Id: install.F,v 1.1.1.1 1996/03/08 15:33:02 mclareni Exp $
*
* $Log: install.F,v $
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
                  macro  install  1=x  2=x  3=x  4=x
*                                                               *
*****************************************************************
*
*-- delete any alias that may interfere with the macro
*
alias/delete *

*
*-- some of the variables used in this script
*
*    NAME     Kind of machine
*    CPU      CPU type (for Apollo only: m68k or a88k)
*    OS       Operating system type: Unix, VMS, VM, NEWLIB
*    FTN      Name of subdirectory for fortran files
*    VERS     Version of cernlib to link with
*    DEV      If -dev then development version, compile with debug flag
*    LIB      If set a stand-alone library will be created, if not
*             the object files will be kept for later use
*
NAME = $lower([1])
A2   = $lower([2])
A3   = $lower([3])
A4   = $lower([4])
OPT  = x
DEV  = x
LIB  = x
VERS = new

if [A2] <> -dev goto endif2
   DEV = [A2]
   goto arg3
endif2:
if [A2] <> -lib goto endif3
   LIB = [A2]
   goto arg3
endif3:
OPT = [A2]
arg3:
if [A3] <> -dev goto endif4
   DEV = [A3]
   goto arg4
endif4:
if [A3] <> -lib goto endif5
   LIB = [A3]
   goto arg4
endif5:
if [A3] = x    goto arg4
goto help
arg4:
if [A4] <> -dev goto endif6
   DEV = [A4]
   goto arg5
endif6:
if [A4] <> -lib goto endif7
   LIB = [A4]
   goto arg5
endif7:
if [A4] = x    goto arg5
goto help
arg5:

*
*-- the user wants help or needs help
*
if [NAME] = ?     goto help
if [NAME] = help  goto help
if [NAME] = x     goto help

if [OPT]  = ?     goto help1
if [OPT]  = help  goto help1

*
*-- set the variables before the actual installation
*
if [NAME] = apollo goto apollo
if [NAME] = sgi    goto unix
if [NAME] = hpux   goto unix
if [NAME] = sun    goto unix
if [NAME] = decs   goto unix
if [NAME] = ibmrt  goto unix
if [NAME] = aix370 goto unix
if [NAME] = cray   goto unix
if [NAME] = ibm    goto ibmvm
if [NAME] = newlib goto newlib
if [NAME] = vax    goto vms
   goto help

apollo:
   CPU = [OPT]
   if [CPU] = x    goto m68k
   if [CPU] = m68k goto unix
   if [CPU] = a88k goto unix
   goto help1
   m68k:
      CPU = m68k
      goto unix

unix:
   OS   = unix
   FTN  = ftn
   goto endif1

ibmvm:
   OS   = vm
   FTN  = M
   goto endif1

newlib:
   OS   = newlib
   FTN  = M
   goto endif1

vms:
   OS   = vms
   FTN  = ftn
   goto endif1

endif1:

*
*-- here follows the actual install stuff
*
mess ' '
mess Installation of CMZ on [NAME] starting at $DATE $TIME .....
mess ' '

exec clean  os=[OS] ftn=[FTN]
exec load   name=[NAME] os=[OS] ftn=[FTN] cpu=[CPU] dev=[DEV]
if [LIB] <> x goto library
   cfor /
   set c   -lan
   cfor /
   set ftn -lan
   goto endif8
library:
   cflib /
   set c   -lan
   cflib /
   set ftn -lan
endif8:

mess ' '
mess 'If you also want to build kuipc type:'
mess exec kuipc  name=[NAME] os=[OS] vers=[VERS] dev=[DEV]
mess ' '

mess ' '
mess Installation of CMZ on [NAME] finished at $DATE $TIME .....
mess ' '

goto end


help:
exec help
goto end

help1:
exec help name=[NAME]
goto end

end:
return

