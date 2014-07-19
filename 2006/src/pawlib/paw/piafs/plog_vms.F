*
* $Id: plog_vms.F,v 1.1.1.1 1996/03/01 11:39:28 mclareni Exp $
*
* $Log: plog_vms.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_VAX)
*CMZ :  2.06/03 21/11/94  11.57.21  by  Julian Bunn
*-- Author :    Julian Bunn   21/11/94
      Subroutine VMSFORLOG(logfile,istat)
c
c For VMS only
c Opens unit 6 in shared mode for the logfile which is
c redirected stdout in C
c
      character*(*) logfile
      integer istat
c
      istat = 0
      lfile = lenocc(logfile)
      print *, 'Trying ...'
      open(6,file=logfile(:lfile),status='old',
     &    access='append',err=999,iostat=istat)
      print *, 'Logfile opened for Fortran'
      return
  999 continue
      end
#endif
