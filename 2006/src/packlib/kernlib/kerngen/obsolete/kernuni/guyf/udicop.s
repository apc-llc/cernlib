*
* $Id: udicop.s,v 1.1.1.1 1996/02/15 17:54:05 mclareni Exp $
*
* $Log: udicop.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:05  mclareni
* Kernlib
*
*
. **************************************
.
.         SUBROUTINE UDICOP( A, X, IDO, IW, NA, NX )   *** V 302 ***
.         SEE UCOCOP
.
. **************************************
 
          AXR$
          FORT$
$(1)
UDICOP*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG         . LOC(IDO), LOC(IW)
          L         R2,0,A2           . IDO
          L         A4,0,A3           . IW
          JGD       R2,$+2
          J         RETUR6            . DO NOTHING IF IDO.LE.0
          DL        A2,4,XARG         . LOC(NA), LOC(NX)
          S         A4,R1
          DL        A0,0,XARG         . LOC(A), LOC(X)
          S         A4,R3
          L         A2,0,A2           . NA
          L         A3,0,A3           . NX
          LXI,U     A0,-1
          JGD       R1,$+2
          J         RETUR6            . DO NOTHING IF IW.LE.0
          ANU       A4,A2             . (A5) = IW - NA
          LXI,U     A1,-1
          A         A0,R1
          AN        A4,A3             . (A4) = IW - NX
          MSI       A2,R2             . NA * (IDO-1)
          A         A1,R1
          MSI       A3,R2             . NX * (IDO-1)
          A         A0,A2             . LOC( A(NA*(IDO-1)+IW) )
          A         A1,A3             . LOC( X(NX*(IDO-1)+IW) )
UDIC40                                . LOOP
          L         R1,R3             . (R1) = IW
          BT        A1,0,*A0
          A         A0,A5
          A         A1,A4
          JGD       R2,UDIC40
          J         RETUR6
          END
#ifdef CERNLIB_TCGEN_UDICOP
#undef CERNLIB_TCGEN_UDICOP
#endif
