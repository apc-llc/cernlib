*
* $Id: iulook.s,v 1.1.1.1 1996/02/15 17:53:14 mclareni Exp $
*
* $Log: iulook.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:14  mclareni
* Kernlib
*
*
IULOOK   CSECT
*
* CERN PROGLIB# M410    IULOOK          .VERSION KERNIBM  2.26  890316
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
IULOOK   AMODE ANY
IULOOK   RMODE ANY
#endif
         EXTRN UBUNCH
         USING *,15
         BC    15,12(15)
         DC    X'7'
         DC    CL7'IULOOK '
         STM   14,12,12(13)
         LR    3,13
         LA    13,SAVEAREA
         ST    13,8(3)
         ST    3,4(13)
         B     BEGIN
         CNOP  0,4
         USING *,13
SAVEAREA DS    18F
ADCON    DC    A(UBUNCH)
ERSTARG  DC    A(SLATE)
         DC    AL4(RESULT)
         DC    AL4(I+X'80000000')
I        DS    1F
BLANKS   DC    CL4'    '
RESULT   DS    1F
BEGIN    LM    2,5,0(1)
         L     12,ERSTARG
         L     11,BLANKS
         ST    11,0(12)
         L     7,0(4)
         L     5,0(5)
         LA    10,1
         SR    4,4
         SR    6,6
         LA    8,4
         MR    6,8                 R7=JL
         MR    4,8                 R5=JR
         L     2,0(2)
         SR    3,8
         SR    4,4                 R4=I
         SR    6,6                 R6=I*4
ST20     L     9,0(3,7)
         CR    9,11
         BE    ST24
         CR    4,2
         BNL   ST29
         AR    4,10
         ST    9,0(12,6)
         AR    6,8
ST24     AR    7,8
         CR    7,5
         BNH   ST20
ST29     SR    6,6
         CR    6,4
         BNE   SKIP
         LR    4,10
SKIP     ST    4,I
         LA    1,ERSTARG
         L     15,ADCON
         BALR  14,15
         L     0,RESULT
         L     13,4(13)
         LM    2,12,28(13)
         L     14,12(13)
         MVI   12(13),X'FF'
         BCR   15,14
SLATE    COM
         DS    40F
         END
#ifdef CERNLIB_TCGEN_IULOOK
#undef CERNLIB_TCGEN_IULOOK
#endif
