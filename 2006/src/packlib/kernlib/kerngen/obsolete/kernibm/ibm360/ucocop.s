*
* $Id: ucocop.s,v 1.1.1.1 1996/02/15 17:53:17 mclareni Exp $
*
* $Log: ucocop.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:17  mclareni
* Kernlib
*
*
UCOCOP   CSECT
*
* CERN PROGLIB# V302    UCOCOP          .VERSION KERNIBM  0.1   720321
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
UCOCOP   AMODE ANY
UCOCOP   RMODE ANY
#endif
         ENTRY UCOCOP
         USING *,15
         BC    15,12(15)
         DC    X'7'
         DC    CL7'UCOCOP '
         STM   14,12,12(13)
         LM    2,7,0(1)
         L     4,0(4)              IDO
         LTR   4,4
         BZ    RETURN
         L     5,0(5)              IW
         LTR   5,5
         BZ    RETURN
         L     9,0(7)              NB
         L     7,0(6)              NA
         SR    7,5                 KK
         SR    9,5                 LL
         SR    6,6
         SR    8,8
         LA    10,4
         MR    6,10
         MR    8,10
         SR    6,6
         SR    8,8
         CNOP  0,8
LOOP1    LR    11,5
LOOP2    L     12,0(2,6)
         ST    12,0(3,8)
         AR    6,10
         AR    8,10
         BCT   11,LOOP2
         AR    6,7
         AR    8,9
         BCT   4,LOOP1
RETURN   DS    0H
         LM    2,12,28(13)
         MVI   12(13),X'FF'
         BCR   15,14
         END   ,
#ifdef CERNLIB_TCGEN_UCOCOP
#undef CERNLIB_TCGEN_UCOCOP
#endif
