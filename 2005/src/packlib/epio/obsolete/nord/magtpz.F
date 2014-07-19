*
* $Id: magtpz.F,v 1.1.1.1 1996/03/08 15:21:59 mclareni Exp $
*
* $Log: magtpz.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:59  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_ND50)
      INTEGER FUNCTION MAGTPZ(MFUNC,IARR,LUNIT,NBYTE,NBYTR,MSTAT)
      DIMENSION IARR(1)
      LENBUF=NBYTE/4
      MF=MFUNC
      IF(MF.EQ.27B)MF=1
      IF(MF.EQ.26B)MF=0
      MAGTPZ=MAGTP(MF,IARR,LUNIT,LENBUF,L)
      NBYTR=L*4
      RETURN
      END
#endif
