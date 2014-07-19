*
* $Id: sxyb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: sxyb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE SXYB(NW,BINV,SCAL,Y,X)
*
*         Y = SCAL*X*Y
*
*********************************************************************
SXYB    CSECT
#if defined(CERNLIB_QMIBMXA)
SXYB    AMODE 31
SXYB    RMODE ANY
#endif
         USING *,15
         STM   0,9,20(13)
* R1 NW, R2 BINV, R3 SCAL , R4 Y , R5 X
         LM    1,5,0(1)
*
*IN R1 NW
         L     1,0(0,1)
         LE    0,0(0,5)
         LR    7,3
*
         VSVMM 1
LOOP     VLVCU 1
         VLVM  2
         VLME  0,4
         VME   2,0,3
         VMEQ  4,0,2
         VSTME 4,7
         BC    2,LOOP
         VSVMM 0
         LM    0,9,20(13)
         BR    14
*
         END
#endif
