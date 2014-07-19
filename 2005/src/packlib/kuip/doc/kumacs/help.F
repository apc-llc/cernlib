*
* $Id: help.F,v 1.1.1.1 1996/03/08 15:33:02 mclareni Exp $
*
* $Log: help.F,v $
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
                  macro  help  name=x
*                                                               *
*****************************************************************

if [name] = apollo goto apollo
if [name] = sgi    goto generic
if [name] = hpux   goto generic
if [name] = sun    goto generic
if [name] = decs   goto generic
if [name] = ibmrt  goto generic
if [name] = aix370 goto generic
if [name] = cray   goto generic
if [name] = ibm    goto generic
if [name] = newlib goto generic
if [name] = vax    goto generic

   mess ' '
   mess 'To install KUIP type the following:'
   mess ' '
   mess '   EXEC INSTALL <machine> [<opt>] [-dev] [-lib]'
   mess ' '
   mess 'Where <machine> may be one of the following:'
   mess '   apollo, sgi, hpux, sun, decs, ibmrt, aix370,'
   mess '   cray, ibm, newlib, vax'
   mess ' '
   mess 'To see the possible values of <opt> for a specific machine type:'
   mess '   EXEC <machine> help   or   EXEC <machine> ?'
   mess ' '
   mess 'The option -dev should only be specified for compiling and'
   mess 'linking in debug (development) mode.'
   mess ' '
   mess 'If option -lib is specified a stand-alone library will be created,'
   mess 'otherwise the object file(s) are kept for later usage.'
   mess ' '
   goto end

apollo:
   mess ' '
   mess 'To install KUIP on the '//[name]//' type:'
   mess ' '
   mess     EXEC INSTALL [name] '[<opt>] [-dev] [-lib]'
   mess ' '
   mess 'Where <opt> may be one of the following:'
   mess '   m68k (default)  to install CMZ on a motorola Apollo'
   mess '   a88k            to install CMZ on a DN10000 Apollo'
   mess ' '
   mess 'The option -dev should only be specified for compiling and'
   mess 'linking in debug (development) mode.'
   mess ' '
   mess 'If option -lib is specified a stand-alone library will be created,'
   mess 'otherwise the object file(s) are kept for later usage.'
   mess ' '
   goto end

generic:
   mess ' '
   mess 'To install KUIP on the '//[name]//' type:'
   mess ' '
   mess     EXEC INSTALL [name] '[-dev] [-lib]'
   mess ' '
   mess 'The option -dev should only be specified for compiling and'
   mess 'linking in debug (development) mode.'
   mess ' '
   mess 'If option -lib is specified a stand-alone library will be created,'
   mess 'otherwise the object file(s) are kept for later usage.'
   mess ' '
   goto end

end:
return
