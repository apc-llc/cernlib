*
* $Id: copyb.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: copyb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE COPYB(NW,BINV,Y,X)
*
*         Y(BV) = X(BV)
*
*********************************************************************
COPYB   CSECT
#if defined(CERNLIB_QMIBMXA)
COPYB   AMODE 31
COPYB   RMODE ANY
#endif
         USING *,15
         STM   0,4,20(13)
* R1 NW, R2 BINV, R3 Y, R4 X
         LM    1,4,0(1)
*
*IN R1 NW
         L     1,0(0,1)
*
LOOP     VLVCU 1
         VLVM  2
         VLME  4,4
         VSTME 4,3
         BC    2,LOOP
         LM    0,4,20(13)
         BR    14
*
         END
#endif
