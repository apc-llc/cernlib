*
* $Id: pstage_vms.F,v 1.1.1.1 1996/03/01 11:39:28 mclareni Exp $
*
* $Log: pstage_vms.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_TGV)
*CMZ :  2.06/03 18/10/94  17.10.03  by  Julian Bunn
*-- Author :    Julian Bunn   18/10/94
      subroutine pstage(chmail,c1,c2,c3)
      character*(*) chmail,c1,c2,c3
      write(6,'(a)') ' VMS stage: '//chmail//' '//c1//' '//c2//' '//c3
      end
#endif
