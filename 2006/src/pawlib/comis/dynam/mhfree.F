*
* $Id: mhfree.F,v 1.5 1997/07/18 15:27:15 berejnoi Exp $
*
* $Log: mhfree.F,v $
* Revision 1.5  1997/07/18 15:27:15  berejnoi
* using displacement from mdpool
*
* Revision 1.4  1997/07/14 11:30:22  couet
* - Use again csfree for sgi and linux.
*
* Revision 1.3  1997/07/08 12:59:52  couet
* - go back to the old dynamic memory allocation mechanism for LINUX. The new
*   one does not work: it hangs in mhfree.
*
* Revision 1.2  1997/06/03 08:51:09  berejnoi
* using csfree
*
* Revision 1.1.1.1  1996/02/26 17:16:59  mclareni
* Comis
*
*
#include "comis/pilot.h"
*-- Author : V.Berezhnoi
      SUBROUTINE MHFREE(I)
C      COMMON /MDIND/ MSTCK,MHEAP
*-------------------------------
      ipointer=(I-1)*4
      call csfree(ipointer)
      return 
*-------------------------------
*      CALL MDFH(MHEAP,I)
*---------
      END
