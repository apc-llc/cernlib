*
* $Id: iulast.s,v 1.1.1.1 1996/02/15 17:53:14 mclareni Exp $
*
* $Log: iulast.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:14  mclareni
* Kernlib
*
*
IULAST   CSECT
*
* CERN PROGLIB# V305    IULAST          .VERSION KERNIBM  0.1   720321
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
IULAST   AMODE ANY
IULAST   RMODE ANY
#endif
         USING *,15
         BC    15,12(15)
         DC    X'7'
         DC    CL7'IULAST '
         STM   14,8,12(13)
         LM    2,4,0(1)
         L     2,0(2)
         L     5,0(4)
         SR    4,4
         LTR   6,5
         BZ    ST20
         LA    7,4
         MR    4,7
ST12     SR    5,7
         L     8,0(3,5)
         CR    2,8
         BNE   ST20
         BCT   6,ST12
ST20     LR    0,6
         LM    2,8,28(13)
         MVI   12(13),X'FF'
         BCR   15,14
         END   ,
#ifdef CERNLIB_TCGEN_IULAST
#undef CERNLIB_TCGEN_IULAST
#endif
