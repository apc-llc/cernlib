*
* $Id: smod3b.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: smod3b.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE SMOD3B(NW,A1,A2,A3,OUT,IBV)
*
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
SMOD3B   CSECT
#if defined(CERNLIB_QMIBMXA)
SMOD3B   AMODE 31
SMOD3B   RMODE ANY
#endif
         USING SMOD3B,12
         STM   G14,G12,12(13)
         LR    12,15
         ST    13,SAVA+4
         LR    2,13
         LA    13,SAVA
         ST    13,8(2)
*GET ADDRESS OF PARM LIST NW , IN , OUT
         LM    1,6,0(1)
*
*IN R1 NW
         L     1,0(0,1)
* NO SECTIONING
*
         LR    7,3
         LR    8,4
*
         VSVMM 1             Set Vector Mask Mode ON
*
LOOPV    VLVCU 1
         VLVM  6
         VLME  2,2
         VMER  14,2,2
         VLME  4,3
         VMAE  14,4,7
         VLME  6,4
         VMAE  14,6,8
         L     15,=V(W#SQRT)
         BALR  14,15
*
         VSTME 0,5
         LTR   G1,G1         Restore proper OC
         BC    2,LOOPV
         VSVMM 0             Set Vector MAsk Mode OFF
* CONCLUSION NO SECTIONING
*
         L     13,SAVA+4
         LM    G14,G12,12(G13)
         BR    14
SAVA     DS    18F
*
         DC    V(VFVIX#)
*
#include "equats.inc"
         END
#endif
