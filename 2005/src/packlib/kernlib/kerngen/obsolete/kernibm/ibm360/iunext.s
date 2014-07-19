*
* $Id: iunext.s,v 1.1.1.1 1996/02/15 17:53:14 mclareni Exp $
*
* $Log: iunext.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:14  mclareni
* Kernlib
*
*
IUNEXT   CSECT
*
* CERN PROGLIB# M410    IUNEXT          .VERSION KERNIBM  0.1   720518
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
IUNEXT   AMODE ANY
IUNEXT   RMODE ANY
#endif
         USING *,15
         BC    15,12(15)
         DC    X'7'
         DC    CL7'IUNEXT '
         STM   14,7,12(13)
         L     2,0(1)
         L     3,4(1)
         L     7,0(3)
         LR    3,7
         LA    4,4
         LA    1,1
         SR    6,6
         MR    6,4
         SR    2,4
         L     5,BLANKS
ST10     L     6,0(2,7)
         CR    5,6
         BNE   ST12
         AR    7,4
         AR    3,1
         B     ST10
ST12     LR    0,3
         LM    2,7,28(13)
         MVI   12(13),X'FF'
         BCR   15,14
         CNOP  0,4
BLANKS   DC    CL4'    '
         END   ,
#ifdef CERNLIB_TCGEN_IUNEXT
#undef CERNLIB_TCGEN_IUNEXT
#endif
