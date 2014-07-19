*
* $Id: dxypwzb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: dxypwzb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE DXYPWZB(NW,BV,S,X,Y,T,V,W)
*
*         Y = S*X(BV)*Y(BV)+T*V(BV)*W(BV)
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
DXYPWZB CSECT
#if defined(CERNLIB_QMIBMXA)
DXYPWZB AMODE 31
DXYPWZB RMODE ANY
#endif
#include "cachesz.inc"
         USING *,15
         STM   0,9,20(13)
* R1 NW, R2 BV, R3 S , R4 Y , R5 X , R6 T , R7 V , R8 W
         LM    1,8,0(1)
*
*IN R1 NW
         L     1,0(0,1)
         LD    0,0(0,3)
         LD    2,0(0,6)
         LR    9,4
*
         VSVMM 1
LOOP     VLVCU 1
         VLVM  2
         VLMD  0,4
         VMD   2,0,5
         VMDQ  4,0,2
* FIRST PART
         VLMD  6,7
         VMD   8,6,8
         VMDQ  10,2,8
         VADR  12,10,4
         VSTMD 12,9
         BC    2,LOOP
         VSVMM 0
         LM    0,9,20(13)
         BR    14
*
         END
#endif
