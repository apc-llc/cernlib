*
* $Id: iulook.s,v 1.1.1.1 1996/02/15 17:54:37 mclareni Exp $
*
* $Log: iulook.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:37  mclareni
* Kernlib
*
*
IULOOK   CSECT
*
* CERN PROGLIB# M410    IULOOK          .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _iulook_
_iulook_ STM   2,1,16(13)
         LR    6,13
         USING STACK,13
         S     13,=A(STEND-STACK)
         ST    6,4(13)
         LR    5,3             AIX: address of arg4
         LR    4,2             AIX: address of arg3
         LR    3,1             AIX: address of arg2
         LR    2,0             AIX: address of arg1
         L     14,=A(DATA)
         USING DATA,14
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
         STM   14,15,MYREGS      Save segment registers
         LM    0,3,ERSTARG
         L     15,=V(_ubunch_)
         BALR  14,15
         LM    14,15,MYREGS      Restore segment registers
         L     0,RESULT
         L     13,4(13)
         LM    2,14,16(13)
         BCR   15,14
         LTORG
DATA     PSECT
         DS    0F
ERSTARG  DC    V(_slate_)
         DC    AL4(RESULT)
         DC    AL4(I)
         DC    F'-1'
I        DS    1F
BLANKS   DC    CL4'    '
RESULT   DS    1F
STACK    DSECT
SAVEAREA DS    18F
AIXOUT   DS    4F
LINKR23  DS    2F
MYREGS   DS    2F
STEND    DS    0F
SLATE    DSECT
         DS    40F
         END
#ifdef CERNLIB_TCGEN_IULOOK
#undef CERNLIB_TCGEN_IULOOK
#endif
