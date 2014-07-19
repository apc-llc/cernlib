*
* $Id: hfreeg.F,v 1.1.1.1 1996/01/16 17:08:09 mclareni Exp $
*
* $Log: hfreeg.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#if defined(CERNLIB_VAX)
#include "hbook/pilot.h"
*CMZ :  4.10/05 29/03/89  14.35.22  by  Rene Brun
*-- Author :
      FUNCTION hfreeg(global_size,base_common,off_set)
*
*       J.G. Loken      1/11/87
*
*       Function to dynamically delete/unmap global section space
*       using the service SYS$DELTVA.
*
*       GLOBAL_SIZE is the size of the section to be freed (pages).
*       BASE_COMMON is the first word of reference COMMON.
*       OFF_SET is the offset from BASE_COMMON of the mapped section in words.
*                       i.e., BASE_COMMON(OFF_SET) is the first word.
*       HFREEG is returned equal to the global section length (pages) if OK,
*               or as an error if not (< 0).
*
      INTEGER*4 global_size,base_common,off_set,hfreeg,sys$deltva
      INTEGER*4 inad(2)
      DIMENSION base_common(*)
*
*
******************************************************************************
*
      inad(1)=%loc(base_common(off_set))
      inad(2)=inad(1)+global_size*512-1
      hfreeg = sys$deltva(inad,,)
      IF((hfreeg.AND.1).EQ.0) THEN
        hfreeg=-hfreeg
      ELSE
        hfreeg=global_size
      ENDIF
      RETURN
      END
 
#endif
