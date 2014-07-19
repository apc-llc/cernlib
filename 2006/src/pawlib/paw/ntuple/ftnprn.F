*
* $Id: ftnprn.F,v 1.3 1996/07/03 13:33:06 couet Exp $
*
* $Log: ftnprn.F,v $
* Revision 1.3  1996/07/03 13:33:06  couet
* - The format was wrong for VAX. 1X was missing
*
* Revision 1.2  1996/06/17 12:34:10  couet
* - A printout of more than 132 characters was done on VAX
*
* Revision 1.1  1996/03/19 14:12:45  maartenb
* - nt/scan now obeys the setting of the output lun.
*
* - Profile histograms get booked with the right option.
*
* - Cache gets flushed when qp_execute() is not succesfull !!
*
      subroutine FTNPRN( chmess )
      character*(*) chmess
#include "paw/pilot.h"
#include "hbook/hcunit.inc"
      ilen = min(len(chmess),131)
      write( lout, 1000 ) chmess(1:ilen)
#if defined(CERNLIB_VAX)
 1000 format(1x,a)
#endif
#if !defined(CERNLIB_VAX)
 1000 format(a)
#endif
      end
