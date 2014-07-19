*
* $Id: uleft.s,v 1.1.1.1 1996/02/15 17:53:17 mclareni Exp $
*
* $Log: uleft.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:17  mclareni
* Kernlib
*
*
ULEFT    CSECT
*
* CERN PROGLIB# M410    ULEFT           .VERSION KERNIBM  1.09  820119
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
ULEFT    AMODE ANY
ULEFT    RMODE ANY
#endif
         USING *,15
         BC    15,10(15)
         DC    X'5'
         DC    CL5'ULEFT'
         STM   14,12,12(13)
         L     2,0(1)
         L     7,4(1)
         L     9,8(1)
         L     7,0(7)
         L     9,0(9)
         LA    1,1
         LA    4,4
         SR    6,6
         SR    8,8
         MR    6,4                 R7=JP
         MR    8,4                 R8=JT
         LR    8,7                 R9=JR
         SR    2,4
         SR    11,11               NC
         L     3,BLANKS
         CNOP  0,8
ST12     CR    8,9
         BH    RETURN1
         L     10,0(2,8)
         CR    10,3
         BE    ST19
         CR    7,8
         BE    ST17
         ST    10,0(2,7)
         ST    3,0(2,8)
ST17     AR    11,1
         AR    7,4
ST19     AR    8,4
         B     ST12
RETURN1  L     12,NC
         ST    11,0(12)
         DR    6,4
         ST    7,4(12)
         LM    2,12,28(13)
         MVI   12(13),X'FF'
         BCR   15,14
         ENTRY URIGHT
         USING *,15
URIGHT   BC    15,12(15)
         DC    X'7'
         DC    CL7'URIGHT '
         STM   14,12,12(13)
         L     2,0(1)
         L     7,8(1)
         L     9,4(1)
         L     7,0(7)
         L     9,0(9)
         LA    1,1
         LA    4,4
         SR    6,6
         SR    8,8
         MR    6,4                 R7=JP
         MR    8,4                 R8=JT
         LR    8,7                 R9=JL
         SR    2,4
         SR    11,11               R11=NC
         L     3,BLANKS
         CNOP  0,8
ST22     CR    8,9
         BL    RETURN2
         L     10,0(2,8)
         CR    10,3
         BE    ST29
         CR    7,8
         BE    ST27
         ST    10,0(2,7)
         ST    3,0(2,8)
ST27     AR    11,1
         SR    7,4
ST29     SR    8,4
         B     ST22
RETURN2  L     12,NC
         ST    11,0(12)
         DR    6,4
         ST    7,4(12)
         LM    2,12,28(13)
         MVI   12(13),X'FF'
         BCR   15,14
NC       DC    A(SLATE)
BLANKS   DC    CL4'    '
SLATE    COM
         DS    40F
         END
#ifdef CERNLIB_TCGEN_ULEFT
#undef CERNLIB_TCGEN_ULEFT
#endif
#ifdef CERNLIB_TCGEN_URIGHT
#undef CERNLIB_TCGEN_URIGHT
#endif
