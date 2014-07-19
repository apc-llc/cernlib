*
* $Id: iuback.s,v 1.1.1.1 1996/02/15 17:53:13 mclareni Exp $
*
* $Log: iuback.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:13  mclareni
* Kernlib
*
*
IUBACK   CSECT
*
* CERN PROGLIB# M410    IUBACK          .VERSION KERNIBM  2.26  890316
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
IUBACK   AMODE ANY
IUBACK   RMODE ANY
#endif
         EXTRN IUCOMP
         USING *,15
         BC    15,12(15)
         DC    X'7'
         DC    CL7'IUBACK '
         STM   14,12,12(13)
         LR    3,13
         LA    13,SAVEAREA
         ST    13,8(3)
         ST    3,4(13)
         B     BEGIN
         ENTRY IUFORW
         USING *,15
IUFORW   BC    15,12(15)
         DC    X'7'
         DC    CL7'IUFORW '
         STM   14,12,12(13)
         LR    3,13
         ST    13,8(3)
         LA    13,SAVEAREA
         ST    3,4(13)
         B     BEGIN2
         CNOP  0,4
         USING *,13
SAVEAREA DS    18F
NUM      DC    CL4'    '
         DC    CL4'0   '
         DC    CL4'1   '
         DC    CL4'2   '
         DC    CL4'3   '
         DC    CL4'4   '
         DC    CL4'5   '
         DC    CL4'6   '
         DC    CL4'7   '
         DC    CL4'8   '
         DC    CL4'9   '
AD11     DC    F'11'
ADCON    DC    A(IUCOMP)
ERSTARG  DC    AL4(ERSTARG)
         DC    AL4(NUM)
         DC    AL4(AD11+X'80000000')
ND       DC    A(SLATE)
BEGIN    L     2,0(1)
         L     7,8(1)
         L     7,0(7)
         L     9,4(1)
         L     9,0(9)
         SR    6,6
         SR    8,8
         LA    12,1                R12=1
         LA    4,4                 R4=4
         MR    6,4                 R7=JEND
         MR    8,4                 R9=JL
         SR    6,6                 R6=0
         LR    10,6
         SR    5,5                 R5=IVAL
         LA    8,10                R8=10
         SR    2,4
         SR    3,3                 R3=ND
         LR    11,12               R11=IPOW
ST12     CR    7,9
         BL    ST20
         LA    15,0(2,7)
         ST    15,ERSTARG
         LA    1,ERSTARG
         L     15,ADCON
         BALR  14,15
         LR    15,0
         SR    15,12
         CR    15,6
         BL    ST20
         BE    ST19
         SR    15,12
         LR    14,6
         MR    14,11
         AR    5,15                IVAL=(K-1)*IPOW+IVAL
         MR    10,8
         AR    3,12
ST19     SR    7,4
         B     ST12
ST20     DR    6,4
         L     12,ND
         ST    7,4(12)
         ST    3,0(12)
         LR    0,5
         L     13,4(13)
         LM    2,12,28(13)
         L     14,12(13)
         MVI   12(13),X'FF'
         BCR   15,14
BEGIN2   L     2,0(1)
         L     7,4(1)
         L     7,0(7)
         L     9,8(1)
         L     9,0(9)
         LA    12,1
         LA    4,4
         SR    6,6
         SR    8,8
         MR    8,4                 R7=JEND
         MR    6,4                 R9=JR
         SR    2,4
         SR    3,3                 R3=ND
         SR    11,11               R11=IVAL
         SR    10,10
         SR    6,6
         LA    8,10                R8=10
ST22     CR    7,9
         BH    ST30
         LA    15,0(2,7)
         ST    15,ERSTARG
         LA    1,ERSTARG
         L     15,ADCON
         BALR  14,15
         LR    15,0
         SR    15,12
         CR    15,6
         BL    ST30
         BE    ST29
         SR    15,12
         MR    10,8
         AR    11,15
         AR    3,12
ST29     AR    7,4
         B     ST22
ST30     LR    5,11
         B     ST20
SLATE    COM
         DS    40F
         END
#ifdef CERNLIB_TCGEN_IUBACK
#undef CERNLIB_TCGEN_IUBACK
#endif
#ifdef CERNLIB_TCGEN_IUFORW
#undef CERNLIB_TCGEN_IUFORW
#endif
