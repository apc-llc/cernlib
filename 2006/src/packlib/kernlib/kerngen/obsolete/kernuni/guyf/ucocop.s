*
* $Id: ucocop.s,v 1.1.1.1 1996/02/15 17:54:05 mclareni Exp $
*
* $Log: ucocop.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:05  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE UCOCOP(A,X,IDO,IW,NA,NX)        *** V 302 ***
.     EXTRACTS IDO TIMES IW CONSECUTIVE WORDS FROM A, EVERY NA WORDS
.     AND PLANTS THEM INTO X, EVERY NX WORDS
.     DO NOTHING IF  IW.LE.0 .OR. IDO.LE.0
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
UCOCOP*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG          . LOC(IDO), LOC(IW)
          L         R2,0,A2            . IDO
          L         A3,0,A3            . IW
          DL        A1,4,XARG
          TLE,U     A3,1
          J         RETUR6             . IF IW.LE.0
          L         A4,A3
          ANU       A4,0,A1            . IW-NA
          DL        A0,0,XARG          . LOC(A), LOC(X)
          AN        A4,0,A2            . IW-NX
          LXI,U     A0,1
          LXI,U     A1,1
          J         UCP2
UCP1
          LR        R1,A3
          BT        A1,0,*A0
          AN        A0,A5
          AN        A1,A4
UCP2      JGD       R2,UCP1
          J         RETUR6
 
#if (defined(CERNLIB_WBACK))&&(defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
          INFO      010  3
$(3)      +         0776003,UCOCOP
          +         0,SAVX11
          +         'UCOCOP'
#include "gerr_c.inc"
SAVX11    RES       1
#endif
          END
#ifdef CERNLIB_TCGEN_UCOCOP
#undef CERNLIB_TCGEN_UCOCOP
#endif
