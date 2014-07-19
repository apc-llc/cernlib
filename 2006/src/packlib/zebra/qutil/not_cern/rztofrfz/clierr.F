*
* $Id: clierr.F,v 1.1.1.1 1996/03/06 10:47:20 mclareni Exp $
*
* $Log: clierr.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:20  mclareni
* Zebra
*
*
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
#if defined(CERNLIB_VAX)
c     ***********************************************************
      integer*4 function cli_error_handler( sigargs, mechargs )
c     ***********************************************************
c     Purpose : Modify the SEVERITY of the CLI$ errors so that
c               they will not cause a TRACEBACK/dump of the
c               code segment.  The SEVERITY bit is set to
c               INFORMATIONAL.
 
      implicit none
c     See the VAX/VMS description of error handlers for details
c     on the calling protocols!
      integer*4 sigargs(*)
      integer*4 mechargs(*)
 
c     Define the SS$_RESIGNAL parameter to resignal the error
c     at its modified severity level:
      include '($ssdef)/list'
 
c     Define the STS$K_INFO parameter so that the severity level
c     may be modified:
      include '($stsdef)/list'
 
c     Modify the SIGARGS(2) severity bit so that it is
c     informational only:
      call mvbits( sts$k_info, 0, 3, sigargs(2), 0 )
 
c     And resignal with this INFORMATIONAL (rather than WARNING)
c     severity level.  This will therefore NOT cause a stack dump
c     and TRACEBACK.
      cli_error_handler= ss$_resignal
      return
      end
 
#endif
