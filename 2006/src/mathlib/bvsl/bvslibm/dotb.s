*
* $Id: dotb.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: dotb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     FUNCTION DOTB(NW,BINV,Y,X,V)
*
*         DOTB = SUM( V(BV)*X(BV) )
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
DOTB    CSECT
#if defined(CERNLIB_QMIBMXA)
DOTB    AMODE 31
DOTB    RMODE ANY
#endif
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
         VLME  2,3
         VLME  4,4
         VMCER 0,2,4
         BC    2,LOOP
         VSVMM 0
         VSPSD 0,0
         LM    0,4,20(13)
         BR    14
*
         END
#endif
