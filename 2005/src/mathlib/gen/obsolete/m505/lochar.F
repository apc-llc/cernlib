*
* $Id: lochar.F,v 1.1.1.1 1996/04/01 15:03:21 mclareni Exp $
*
* $Log: lochar.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:21  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      FUNCTION LOCHAR(IST,L,LINE,N)
      DIMENSION LINE(2),IST(2)
C  LOCATE A CHARACTER STRING IST OF LENGTH L IN TARGET ARRAY LINE OF LEN
C  T LINDELOF 15/9 1977
#if defined(CERNLIB_CDC)
      DATA NCPW/10/,IBL/1H /
#endif
#if defined(CERNLIB_CONVEX64)
      DATA NCPW/ 8/,IBL/8H        /
#endif
#if defined(CERNLIB_CRAY)
      DATA NCPW/ 8/,IBL/1H /
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION JST,IBL,LOOK
#endif
#if (defined(CERNLIB_DOUBLE))&&(!defined(CERNLIB_F90))
      DATA NCPW/ 8/,IBL/8H        /
#endif
#if (defined(CERNLIB_DOUBLE))&&(defined(CERNLIB_F90))
      INTEGER :: NCPW = 8
      IBL = transfer('        ', 0D0)
#endif
      LOCHAR=0
      IF(L.EQ.0) RETURN
      LL=MIN(ABS(L),NCPW)
      IF(LL.GT.N) RETURN
      JST=IBL
      CALL CHMOVE(IST,1,JST,1,LL)
      LOOK=IBL
      NN=N-LL+1
      DO 1 I=1,NN
      CALL CHMOVE(LINE,I,LOOK,1,LL)
      IF(JST.EQ.LOOK) IF(L) 1,1,2
      IF(L) 2,2,1
    1 CONTINUE
      RETURN
    2 LOCHAR=I
      RETURN
      END
