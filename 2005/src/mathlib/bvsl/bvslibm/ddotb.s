*
* $Id: ddotb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: ddotb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     FUNCTION DDOTB(NW,BINV,Y,X,V)
*
*         DDOTB = SUM( V(BV)*X(BV) )
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
DDOTB   CSECT
#if defined(CERNLIB_QMIBMXA)
DDOTB   AMODE 31
DDOTB   RMODE ANY
#endif
#include "cachesz.inc"
         USING *,15
         STM   0,4,20(13)
* R1 NW, R2 BINV, R3 Y, R4 X
         LM    1,4,0(1)
*
*IN R1 NW
         L     1,0(0,1)
         SDR   0,0
*
         VZPSD 0
         VSVMM 1
LOOP     VLVCU 1
         VLVM  2
         VLMD  2,3
         VLMD  4,4
         VMCDR 0,2,4
         BC    2,LOOP
         VSVMM 0
         VSPSD 0,0
         LM    0,4,20(13)
         BR    14
*
         END
#endif
