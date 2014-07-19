*
* $Id: iuchan.s,v 1.1.1.1 1996/02/15 17:53:13 mclareni Exp $
*
* $Log: iuchan.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:13  mclareni
* Kernlib
*
*
IUCHAN   CSECT
*
* CERN PROGLIB# Y201    IUCHAN          .VERSION KERNIBM  1.05  790205
*
*        CODE OF IUBIN ADAPTED FOR IUCHAN    A.BERGLUND, 5-FEB-79
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
IUCHAN   AMODE ANY
IUCHAN   RMODE ANY
#endif
         USING *,15
         B     BEGIN
         DC    X'5'
         DC    CL7'IUCHAN'
BEGIN    STM   1,5,24(13)
         L     2,4(1)             ADDRESS OF XLOW
         L     3,8(1)             ADDRESS OF DX
         L     4,12(1)            ADDRESS OF NX
         L     1,0(1)             ADDRESS OF X
         SDR   0,0
         LE    0,0(1)
         SE    0,0(2)
         DE    0,0(3)                   F0=(X-XLOW)/DX
         AE    0,ONEPE             ADD ONE PLUS EPSILON
         CE    0,ONE
         BL    UNDFL
         CE    0,MAXNUM
         BNH   OK
         LE    0,MAXNUM
OK       L     5,0(4)
         LA    5,1(5)                   R5=NX+1
         AW    0,FOURE
         STD   0,TWO
         L     0,TWO+4                  R0=F0 IN INTEGER FORM
         CR    0,5
         BNM   OVFL
         B     RET
UNDFL    SR    5,5
OVFL     LR    0,5
RET1     EQU   *
RET      EQU   *
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
#ifdef CERNLIB_TCGEN_IUCHAN
#undef CERNLIB_TCGEN_IUCHAN
#endif
