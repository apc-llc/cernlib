*
* $Id: xinbsmlj.s,v 1.1.1.1 1996/02/15 17:53:08 mclareni Exp $
*
* $Log: xinbsmlj.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:08  mclareni
* Kernlib
*
*
*                       .
XINBS    CSECT
*
* CERN PROGLIB# Z203    XINBS  ML       .VERSION KERNIBM  2.16  860113
*
* THIS VERSION IS FOR USE WITH THE SIEMENS/FUJITSU LIBRARY ONLY
*
* CALLING SEQUENCE:     CALL XINBS(LUN,XAV,NA,XBV,NXB)
*
* FUNCTION:     THIS ROUTINE CALLS THE FORTRAN LIBRARY TO PERFORM A
*               READ OF THE FORM:
*                    READ(LUN,END=10,ERR=20) N,(XAV(I),I=1,NA),
*                                              (XBV(J),J=1,L)
*               WHERE L=MIN0(NXB,N) IF N > 0
*                     L=1           IF N < 1
*
* ARGUMENTS:     LUN - FORTRAN DATA SET REFERENCE NUMBER
*                XAV - ARRAY TO CONTAIN DATA
*                NA  - NUMBER OF FULLWORDS TO BE PLACED IN XAV
*                XBV - ARRAY TO CONTAIN DATA
*                NXB - ON ENTRY: MAXIMUM NUMBER OF FULLWORDS
*                                WHICH CAN BE PLACED IN XBV
*                      ON RETURN: THE NUMBER OF WORDS ACTUALLY
*                                 TRANSFERRED TO XBV IF NXB > 0
*                                 EOF READ IF NXB = 0
*                                 READ ERROR IF NXB = -7
*
*********************************************************************
*
*            R.MATTHEWS, CERN/DD, 31-OCTOBER-85
*
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*---------
         PRINT NOGEN
*
*  I N I T I A L I S A T I O N   S E Q U E N C E
*
         USING XINBS,R15
         B     SAVEREGS
         DC    AL1(7),CL7'XINBS'     IDENTIFIER
         DC    C'2.00'               VERSION.MODIFICATION LEVEL
SAVEREGS STM   R14,R12,12(R13)
         LR    R4,R13
         CNOP  0,4
         BAL   R13,AROUNDSA
SAVEAREA DC    A(EPILOGUE),17F'0'
         DROP  R15
         USING SAVEAREA,R13
AROUNDSA ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
*
         LR    R11,R1         R11 -> ARGUMENT LIST
         LM    R6,R10,0(R11)  R6 -> LUN;  R7 -> XAV;  R8-> NA
*                             R9 -> XBV;  R10-> NXB
         ST    R6,PARM1+4     STORE THE ADDRESS OF LUN
         ST    R6,PARM4+4     STORE THE ADDRESS OF LUN FOR ERR EXIT
         ST    R7,PARM5+12    STORE THE ADDRESS OF XAV
         L     R8,0(0,R8)     R8 := NA
         ST    R8,PARM5+4     STORE NA
         ST    R9,PARM3+12    STORE THE ADDRESS OF XBV
         L     R5,=V(JZLSUFI#)  R5 -> JZLSUFI#
*
*  I N I T I A L I S A T I O N   C A L L
*
         LR    R15,R5         R15 -> JZLSUFI#
         LA    R1,PARM1
         BALR  R14,R15
*
*  R E A D   N
*
         LR    R15,R5         R15 -> JZLSUFI#
         LA    R1,PARM2
         BALR  R14,R15
*
*  R E A D   X A V
*
         LR    R15,R5         R15 -> JZLSUFI#
         LA    R1,PARM5
         BALR  R14,R15
*
*  C A L C U L A T E   L
*
         L     R0,N           R0 := N
         LTR   R0,R0          N > 0 ?
         BP    MIN0           YES, BRANCH
         LA    R0,1           L = 1
         B     STOREL
MIN0     L     R1,0(0,R10)    R1 := NXB
         CR    R0,R1          N < NXB ?
         BNH   STOREL         NO, L = N
         LR    R0,R1          L = NXB
STOREL   ST    R0,PARM3+4     STORE L IN READ PARAMETER LIST
         ST    R0,0(0,R10)    STORE L IN CALLER'S ARGUMENT
*
*  R E A D   X B V
*
         LR    R15,R5         R15 -> JZLSUFI#
         LA    R1,PARM3
         BALR  R14,R15
*
         B     EPILOGUE
*
*  E O F   E X I T
*
EOF      EQU   *
         XC    0(4,R10),0(R10)  NXB := 0
         B     EPILOGUE
*
*  E R R   E X I T
*
ERR      EQU   *
         MVC   0(4,R10),ERRFLAG  NXB := ERRFLAG
         LR    R15,R5         R15 -> JZLSUFI#
         LA    R1,PARM4
         BALR  R14,R15
         B     EPILOGUE
*---------
*  R E T U R N   S E Q U E N C E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         OI    12(R13),X'FF'
         BR    R14
*
ERRFLAG  DC    F'-7'
JUNK     DS    F
N        DS    F
*
PARM1    DC    X'14000000'
         DS    A
         DC    A(0)
         DC    A(ERR)
         DC    A(EOF)
*
PARM2    DC    X'04050004'
         DC    A(N)
*
PARM3    DC    X'0E000101'
         DS    F
         DC    X'00270004'
         DS    A
         DC    F'4'
*
PARM4    DC    X'10000000'
         DS    A
         DC    3A(0)
         DC    X'06050004'
         DC    A(JUNK)
*
PARM5    DC    X'0C000101'
         DS    F
         DC    X'00270004'
         DS    A
         DC    F'4'
*
         LTORG
*
         END
#ifdef CERNLIB_XVECTIBM_XINBSML
#undef CERNLIB_XVECTIBM_XINBSML
#endif
#ifdef CERNLIB_XVECTIBM_XINBS
#undef CERNLIB_XVECTIBM_XINBS
#endif
#ifdef CERNLIB_XVECT_XINBS
#undef CERNLIB_XVECT_XINBS
#endif
