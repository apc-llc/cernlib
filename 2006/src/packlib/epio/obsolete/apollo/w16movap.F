*
* $Id: w16movap.F,v 1.1.1.1 1996/03/08 15:21:50 mclareni Exp $
*
* $Log: w16movap.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:50  mclareni
* Epio
*
*
* IBM version working on Apollo
#include "epio/pilot.h"
#if defined(CERNLIB_APOLLO)
      SUBROUTINE W16MOV(SOURCE,N1,TARGET,N2,N3)
C.+++++++ IBM VERSION ++++++++++
C.  MOVES 16 BIT WORDS IN PACKED FORM.
C.
C. INPUT
C. SOURCE     SOURCE ARRAY
C. N1         STARTING 16 BIT WORD IN SOURCE
C. TARGET     TARGET ARRAY
C. N2         STARTING 16 BIT WORD IN TARGET
C. N3         NO. OF 16 BIT WORDS TO MOVE
C.
      INTEGER*2 SOURCE(2),TARGET(2)
      IF(N3.LE.0)  GOTO 77777
      I1=N1-1
      I2=N2-1
      DO 1  I=1,N3
    1 TARGET(I2+I)=SOURCE(I1+I)
77777 RETURN
      END
#endif
