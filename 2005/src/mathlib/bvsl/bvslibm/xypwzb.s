*
* $Id: xypwzb.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: xypwzb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE XYPWZB(NW,BV,S,X,Y,T,V,W)
*
*         Y = S*X(BV)*Y(BV)+T*V(BV)*W(BV)
*
*********************************************************************
XYPWZB  CSECT
#if defined(CERNLIB_QMIBMXA)
XYPWZB  AMODE 31
XYPWZB  RMODE ANY
#endif
         USING *,15
         STM   0,9,20(13)
* R1 NW, R2 BV, R3 S , R4 Y , R5 X , R6 T , R7 V , R8 W
         LM    1,8,0(1)
*
*IN R1 NW
         L     1,0(0,1)
         LE    0,0(0,3)
         LE    2,0(0,6)
         LR    9,4
*
         VSVMM 1
LOOP     VLVCU 1
         VLVM  2
         VLME  0,4
         VME   2,0,5
         VMEQ  4,0,2
* FIRST PART
         VLME  6,7
         VME   8,6,8
         VMEQ  10,2,8
         VAER  12,10,4
         VSTME 12,9
         BC    2,LOOP
         VSVMM 0
         LM    0,9,20(13)
         BR    14
*
         END
#endif
