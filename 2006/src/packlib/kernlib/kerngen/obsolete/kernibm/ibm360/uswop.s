*
* $Id: uswop.s,v 1.1.1.1 1996/02/15 17:53:18 mclareni Exp $
*
* $Log: uswop.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:18  mclareni
* Kernlib
*
*
USWOP    CSECT
*
* CERN PROGLIB# V303    USWOP           .VERSION KERNIBM  0.1   720321
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
USWOP    AMODE ANY
USWOP    RMODE ANY
#endif
         USING *,15
         BC    15,10(15)
         DC    X'5'
         DC    CL5'USWOP'
         STM   14,8,12(13)
         L     2,0(1)
         L     3,4(1)
         L     4,8(1)
         L     4,0(4)
         LTR   4,4
         BZ    RETURN
         LA    6,4
         SR    5,5
NOCHMAL  L     7,0(2,5)
         L     8,0(3,5)
         ST    7,0(3,5)
         ST    8,0(2,5)
         AR    5,6
         BCT   4,NOCHMAL
RETURN   DS    0H
         LM    2,8,28(13)
         MVI   12(13),X'FF'
         BCR   15,14
         END   ,
#ifdef CERNLIB_TCGEN_USWOP
#undef CERNLIB_TCGEN_USWOP
#endif
