*
* $Id: iubin.s,v 1.1.1.1 1996/02/15 17:53:13 mclareni Exp $
*
* $Log: iubin.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:13  mclareni
* Kernlib
*
*
IUBIN    CSECT
*
* CERN PROGLIB# Y202    IUBIN           .VERSION KERNIBM  1.05  790205
*
*        CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*        MODIFIED FOR EPSILON           A.BERGLUND, 5-FEB-79
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
IUBIN    AMODE ANY
IUBIN    RMODE ANY
#endif
         USING *,15
         B     BEGIN
         DC    X'5'
         DC    CL5'IUBIN'
BEGIN    STM   1,5,24(13)
         LM    1,3,0(1)                 R1->X,R2->PAR,R3->SPILL
         SR    4,4                      R4=FALSE
         SDR   0,0
         LE    0,0(1)
         SE    0,8(2)
         DE    0,4(2)                   F0=(X-XLOW)/DX
         AE    0,ONEPE                  ADD  1. + EPSILON
         CE    0,ONE
         BL    UNDFL
         CE    0,MAXNUM
         BNH   OK
         LE    0,MAXNUM
OK       L     5,0(2)
         LA    5,1(5)                   R5=NX+1
         AW    0,FOURE
         STD   0,TWO
         L     0,TWO+4                  R0=F0 IN INTEGER FORM
         CR    0,5
         BNM   OVFL
         B     RET
UNDFL    SR    5,5
OVFL     LR    0,5
RET1     LA    4,1(4)
RET      ST    4,0(3)                  R4=TRUE
         LM    1,5,24(13)
         SR    15,15
         MVI   12(13),X'FF'
         BR    14
         DS    0D
TWO      DS    D
FOURE    DC    X'4E000000'
         DC    F'0'
ONE      DC    X'41100000'
ONEPE    DC    E'1.00001'               1. + EPSILON
MAXNUM   DC    X'487FFFFF'
         END
#ifdef CERNLIB_TCGEN_IUBIN
#undef CERNLIB_TCGEN_IUBIN
#endif
