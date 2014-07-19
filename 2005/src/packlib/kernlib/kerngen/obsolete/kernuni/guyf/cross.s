*
* $Id: cross.s,v 1.1.1.1 1996/02/15 17:54:02 mclareni Exp $
*
* $Log: cross.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:02  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE CROSS(A,B,C)                    *** F 117 ***
.     DIMENSION A(3),B(3),C(3)
.     C = CROSS PRODUCT A*B
. *   SINGLE PRECISION A,B,C
. *   C MAY OVERLAP A OR/AND B
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
CROSS*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A1,0,XARG          . LOC(A), LOC(B)
          DS        A6,R1
          L         A5,2,A1            . A(3)
          L         A3,2,A2            . B(3)
          FM        A5,1,A2            . A(3)*B(2)
          FM        A3,1,A1            . B(3)*A(2)
          L         A6,0,A1            . A(1)
          FAN       A3,A5              . C(1)=A(2)*B(3)-A(3)*B(2)
          FM        A6,2,A2            . A(1)*B(3)
          L         A4,2,A1            . A(3)
          L         A0,2,XARG          . LOC(C)
          FM        A4,0,A2            . A(3)*B(1)
          FAN       A4,A6              . C(2)=A(3)*B(1)-A(1)*B(3)
          L         A6,1,A1            . A(2)
          L         A1,0,A1            . A(1)
          FM        A6,0,A2            . A(2)*B(1)
          FM        A1,1,A2            . A(1)*B(2)
          DS        A3,0,A0
          FAN       A1,A6              . C(3)=A(1)*B(2)-A(2)*B(1)
          DL        A6,R1
          S         A1,2,A0            . C(3)
          J         RETUR3
 END
#ifdef CERNLIB_TCGEN_CROSS
#undef CERNLIB_TCGEN_CROSS
#endif
