*
* $Id: iubin.s,v 1.1.1.1 1996/02/15 17:54:37 mclareni Exp $
*
* $Log: iubin.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:37  mclareni
* Kernlib
*
*
IUBIN    CSECT
*
* CERN PROGLIB# Y202    IUBIN           .VERSION KERNIBX  1.01  900523
*
*    CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*    MODIFIED FOR EPSILON           A.BERGLUND, 5-FEB-79
*    Modified for AIX, Roger Howard, January 1990
         USING *,15
         ENTRY _iubin_
_iubin_  STM   2,5,16(13)
         LR    4,13                     ... added for AIX
         USING STACK,13
         S     13,=A(STEND-STACK)       ... added for AIX
         ST    4,4(0,13)                ... added for AIX
         LR    3,2                      AIX: address of arg3 = SPILL
         LR    2,1                      AIX: address of arg2 = PAR
         LR    1,0                      AIX: address of arg1 = X
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
         L     13,4(0,13)              ... added for AIX
         LM    2,5,16(13)
         BR    14
         DS    0D
FOURE    DC    X'4E000000'
         DC    F'0'
ONE      DC    X'41100000'
ONEPE    DC    E'1.00001'               1. + EPSILON
MAXNUM   DC    X'487FFFFF'
         LTORG
STACK    DSECT
SAVEAREA DS    18F
AIXOUT   DS    4F
LINKR23  DS    2F
TWO      DS    D
STEND    DS    0D
         END
#ifdef CERNLIB_TCGEN_IUBIN
#undef CERNLIB_TCGEN_IUBIN
#endif
