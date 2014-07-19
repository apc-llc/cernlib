*
* $Id: btswapib.F,v 1.1.1.1 1996/03/08 15:21:51 mclareni Exp $
*
* $Log: btswapib.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:51  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (defined(CERNLIB_IBM)||defined(CERNLIB_NORD)||defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_QMIBMVF))
      SUBROUTINE BTSWAP(IARRAY,NBYTES)
C.+++++++ IBM NORD100/500 AND APOLLO VERSION ++++++++++
C.  MOVES 16 BIT WORDS IN PACKED FORM.
C.
C. ARGUMENTS
C. IARRAY     SOURCE ARRAY
C. NBYTES     NO. OF BYTES TO SWAP, STARTING AT THE LEFTMOST
C.            POSITION IN IARRAY. IF LE 1, NO ACTION. IF ODD,
C.            NBYTES-1 IS USED INSTEAD.
C.
      INTEGER*2 IARRAY(2),I2(2)
      INTEGER*4 I4,IACC
      EQUIVALENCE (I2(1),I4)
      N=NBYTES/2
      IF(N.LE.0)  GOTO 999
      DO 1  I=1,N
      I4=0
      I2(2)=IARRAY(I)
      I4=256*I4
      IACC=I2(1)
      I2(1)=0
      I4=I4+IACC
      IARRAY(I)=I2(2)
    1 CONTINUE
  999 RETURN
      END
#endif
