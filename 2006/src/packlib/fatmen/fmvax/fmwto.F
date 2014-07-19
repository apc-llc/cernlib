*
* $Id: fmwto.F,v 1.1.1.1 1996/03/07 15:18:23 mclareni Exp $
*
* $Log: fmwto.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      subroutine fmwto(string,len,irc)
      character*1 string(140)
c
      parameter  (dsc$k_class_s = 1)
      parameter  (dsc$k_dtype_t = 14)
      parameter  (ibufln = 128)
      integer*4  sys$sndopr
      include    '($ssdef)'
      include    '($opcdef)'
      integer*4  desc(2)
      integer*4  opstring(ibufln)
      logical*1  byte(128)
      equivalence (byte(1),opstring(1))
 
      desc(1) = (dsc$k_class_s)*2**24 + (dsc$k_dtype_t)*2**16 + ibufln
      desc(2) = %loc(byte(1))
 
      do i=1,128
         byte(i) = 0
      end do
 
      opstring(1) = (opc$_rq_rqst .or. (opc$m_nm_centrl*256))
 
      do i = 1,len
         byte(i+8) = ichar(string(i))
      end do
 
      istat = sys$sndopr(desc,,)
#include "fatmen/fatvaxrc.inc"
      return
      end
