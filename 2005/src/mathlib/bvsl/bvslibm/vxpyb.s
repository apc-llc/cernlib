*
* $Id: vxpyb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: vxpyb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE VXPYB(NW,BINV,Y,X,V)
*
*         Y(BV) = Y(BV) + V(BV)*X(BV)
*
*********************************************************************
VXPYB   CSECT
#if defined(CERNLIB_QMIBMXA)
VXPYB   AMODE 31
VXPYB   RMODE ANY
#endif
         USING *,15
         STM   0,7,20(13)
* R1 NW, R2 BINV, R3 Y, R4 X, R5 V
         LM    1,5,0(1)
*
*IN R1 NW
         L     1,0(0,1)
         LR    7,3
*
         VSVMM 1
LOOP     VLVCU 1
         VLVM  2
         VLME  0,3
         VLME  4,4
         VMAE  0,4,5
         VSTME 0,7
         BC    2,LOOP
         VSVMM 0
         LM    0,7,20(13)
         BR    14
*
         END
#endif
