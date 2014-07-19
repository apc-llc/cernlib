*
* $Id: vsetb.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: vsetb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE VSETB(NW,BINV,Y,CONST)
*
*       SET Y TO CONST FOR GIVEN INDEX
*
*********************************************************************
VSETB   CSECT
#if defined(CERNLIB_QMIBMXA)
VSETB   AMODE 31
VSETB   RMODE ANY
#endif
         USING *,15
         STM   0,4,20(13)
* R1 NW, R2 BINV, R3 Y,  R4 ALPHA
         LM    1,4,0(1)
*
*IN R1 NW
         L     1,0(0,1)
*IN F4 CONST
         LE    4,0(0,4)
*
LOOP     VLVCU 1
         VLVM  2
         VLMEQ 0,4
         VSTME 0,3
         BC    2,LOOP
         VSVMM 0
         LM    0,4,20(13)
         BR    14
*
         END
#endif
