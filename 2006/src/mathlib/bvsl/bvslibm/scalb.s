*
* $Id: scalb.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: scalb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE SCALB(NW,BINV,Y,ALPHA)
*
*         Y = ALPHA*X
*
*********************************************************************
SCALB   CSECT
#if defined(CERNLIB_QMIBMXA)
SCALB   AMODE 31
SCALB   RMODE ANY
#endif
         USING *,15
         STM   0,7,20(13)
* R1 NW, R2 BINV, R3 Y, R4 ALPHA
         LM    1,4,0(1)
*
*IN R1 NW
         L     1,0(0,1)
*IN F6 ALPHA
         LE    6,0(0,4)
         LR    7,3
*
         VSVMM 1
LOOP     VLVCU 1
         VLVM  2
         VLME  0,3
         VMEQ  2,6,0
         VSTME 2,7
         BC    2,LOOP
         VSVMM 0
         LM    0,7,20(13)
         BR    14
*
         END
#endif
