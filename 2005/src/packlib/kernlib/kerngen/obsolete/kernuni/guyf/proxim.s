*
* $Id: proxim.s,v 1.1.1.1 1996/02/15 17:54:05 mclareni Exp $
*
* $Log: proxim.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:05  mclareni
* Kernlib
*
*
. **************************************
.
.     FUNCTION PROXIM( X, S )
.     RETURNS THE VALUE  X + 2*N*PI  CLOSEST TO  S
.
. **************************************
 
          AXR$
          FORT$
 
$(0)
PI        +         3.1415926536
BIAS      +         0233000000000
 
$(1),FIRS01
PROXIM*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A0,0,XARG         . LOC(X), LOC(S)
          L         A0,0,A0           . X
          L         A5,PI
          L         R2,0,A1           . S
          L         A3,A0
          FAN       A3,R2             . X - S
          LM        A2,A3
          TG        A5,A2
          J         RETUR2
          L         A0,A3
          FD        A3,A5             . (X-S) / PI
          FA        A3,BIAS
          LUF       A1,A3
          FM        A3,A5
          AN,U      A1,0200
          LM        A2,A2
          LSSL      A2,8,A1
          FAN       A0,A3
          JP        A2,PROX30
          TN        A0
          LN        A5,A5
          FA        A0,A5
PROX30
          FA        A0,R2
          J         RETUR2
          END
#ifdef CERNLIB_TCGEN_PROXIM
#undef CERNLIB_TCGEN_PROXIM
#endif
