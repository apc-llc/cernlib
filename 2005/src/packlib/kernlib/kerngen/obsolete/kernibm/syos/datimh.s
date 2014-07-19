*
* $Id: datimh.s,v 1.1.1.1 1996/02/15 17:53:21 mclareni Exp $
*
* $Log: datimh.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:21  mclareni
* Kernlib
*
*
DATIMH   CSECT
*
* CERN PROGLIB# Z007    DATIMH          .VERSION KERNIBM  1.11  820328
*
*******************************************************************
*
*    A. BERGLUND / CERN      DATE: 28/02/80
*
*    CALL DATIMH(ND,NT) RETURNS HOLLERITH DATE + TIME
*                ND=8HDD/MM/YY
*                NT=8HHH.MM.SS
*
*******************************************************************
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
DATIMH   AMODE ANY
DATIMH   RMODE ANY
#endif
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
*
*
*  P R O L O G U E
*
         USING DATIMH,R15
         B     SAVEREGS
         DC    AL1(7),CL7'DATIMH'    IDENTIFIER
         DC    C'1.00'               VERSION.MODIFICATION LEVEL
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
*
         TIME  BIN
         LR    R9,R1         SAVE DATE
         L     R1,4(R11)               ADDRESS OF TIME PART
         BAL   R14,TIME      CONVERT TO PRINTABLE
         LR    R0,R9
         L     R1,0(R11)               ADDRESS OF DATE PART
         BAL   R14,DATE      CONVERT TO PRINTABLE
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         OI    12(R13),X'FF'
         BR    R14
*
DATE     LA    R6,DW                   FIND DOUBLE WORD
         N     R6,=XL4'FFFFF8'
         ST    0,DEC+4                 PLANT DATE
         UNPK  DEC(3),DEC+5(2)         CONVERT YEAR
         MVC   6(2,1),DEC              PUT INTO AREA
         PACK  DEC(8),6(2,1)       REPACK IT
         CVB   R3,DEC              CONVERT TO BINARY
         ST    0,DEC+4             STORE ORIG VALUE AGAIN
         XC    DEC(6),DEC              CLEAR FOR CVB
         NI    DEC+7,X'F0'             SET SIGN
         OI    DEC+7,X'0C'
         CVB   R2,DEC                  DAY OF YEAR
         CL    R2,=A(31+28)            BEFORE FEB 28?
         BNH   LEAP                    BR IF SO
         N     R3,=F'3'                    FOR
         BZ    LEAP                    BR IF SO
         LA    R2,1(,R2)               ALLOW FOR FEB 29
LEAP     LA    R4,10                   SET MONTH TO JAN
         LA    R3,MONTHS
ML       CH    R2,0(,R3)               THIS MONTH?
         BNH   MF                      BR IF SO
         SH    R2,0(,R3)               R3Y NEXT MONTH
         LA    R4,10(,R4)
         LA    R3,2(,R3)
         B     ML
*
MF       CVD   R4,DEC                  CONVERT MONTH
         UNPK  3(3,1),DEC+6(2)
         MH    R2,=H'10'
         CVD   R2,DEC                  CONVERT DAY
         UNPK  0(3,1),DEC+6(2)
         MVI   2(1),C'/'
         MVI   5(1),C'/'
         BR    14
*  CONVERT TIME TO PRINTABLE FORMAT
*
TIME     LA    R6,DW                   FIND DOUBLE WORD
         N     R6,=XL4'FFFFF8'
         LR    R3,0
         SR    R2,R2
         D     R2,=F'100'              DISPOSE OF 100THS
         SR    R2,R2
         D     R2,=F'60'               GET SECONDS
         MH    R2,=H'10'
         CVD   R2,DEC
         UNPK  DEC(3),DEC+6(2)
         MVC   6(2,1),DEC
         SR    R2,R2
         D     R2,=F'60'               GET MINUTES
         MH    R2,=H'10'
         CVD   R2,DEC
         UNPK  3(3,1),DEC+6(2)
         MH    R3,=H'10'
         CVD   R3,DEC                  HOURS
         UNPK  0(3,1),DEC+6(2)
         MVI   2(1),C'.'
         MVI   5(1),C'.'
         BR    14
* CONSTANTS
*
MONTHS   DC    H'31,29,31,30,31,30,31,31,30,31,30,31,999'
DW       DS    2F                      DOUBLE WORD
*
*
DEC      DS    D
*  D A T A   A R E A S
*
         LTORG
*
         END
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
