*
* $Id: mhloc.F,v 1.6 1997/10/23 11:49:49 mclareni Exp $
*
* $Log: mhloc.F,v $
* Revision 1.6  1997/10/23 11:49:49  mclareni
* Simplification on all platforms, necessary for NT
*
* Revision 1.5  1997/07/18 15:26:36  berejnoi
* using displacement from mdpool
*
* Revision 1.4  1997/07/14 11:30:23  couet
* - Use again csfree for sgi and linux.
*
* Revision 1.3  1997/07/08 12:59:54  couet
* - go back to the old dynamic memory allocation mechanism for LINUX. The new
*   one does not work: it hangs in mhfree.
*
* Revision 1.2  1997/06/03 08:50:25  berejnoi
* using malloc
*
* Revision 1.1.1.1  1996/02/26 17:16:59  mclareni
* Comis
*
*
#include "comis/pilot.h"
*-- Author : V.Berezhnoi
      FUNCTION MHLOC(N)
*      COMMON /MDIND/ MSTCK,MHEAP
*----------------------------------
C      COMMON/MDPOOL/IQ(6)
      INTEGER CSALLO
      L=N*4
      IM=CSALLO(L)
      MHLOC=IM/4+1
*----------
*#if defined(CERNLIB_ALIGN)
*      NN=N
*      IF(MOD(NN,2).EQ.1) NN=NN+1
*      MHLOC=MDLH(MHEAP,NN)
*#endif
*#if !defined(CERNLIB_ALIGN)
*      MHLOC=MDLH(MHEAP,N)
*#endif
*----------
      END
