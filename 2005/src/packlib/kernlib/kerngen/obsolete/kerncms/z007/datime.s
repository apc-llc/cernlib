*
* $Id: datime.s,v 1.1.1.1 1996/02/15 17:51:47 mclareni Exp $
*
* $Log: datime.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:47  mclareni
* Kernlib
*
*
         PRINT NOGEN
DATIME   CSECT
#if defined(CERNLIB_QMIBMXA)
DATIME   AMODE ANY
DATIME   RMODE ANY
#endif
***********************************************************************
*                                                                     *
* CERN Program Library Z007 for VM/CMS                                *
*                                                                     *
*       CALL DATIME(IDAY,ITIME)                                       *
*                                                                     *
*       IDAY  returned as YYMMDD                                      *
*       ITIME          as   HHMM                                      *
*                                                                     *
***********************************************************************
*                                                                     *
* Modification Log                                                    *
*                                                                     *
* Original:  Tony Cass  CERN/DD  March 1989                           *
*                                                                     *
*     1.01:  Fill variables in /SLATE/                                *
*            Tony Cass  CERN/DD  October 1989                         *
*                                                                     *
***********************************************************************
         USING DATIME,R15
         B     SAVEREGS
         DC    AL1(7),CL7'DATIME' Name
         DC    CL4'1.01'          Version.modification
SAVEREGS STM   R14,R12,12(R13)
         LR    R4,R13
         CNOP  0,4
         BAL   R13,CARRYON
SAVEAREA DC    18F'0'
         DROP  R15
         USING SAVEAREA,R13
CARRYON  ST    R13,8(,R4)
         ST    R4,R4(,R13)
         XC    8(4,R13),8(R13)
*
         LM    R1,R2,0(R1)         Get Arg addresses
         LA    R3,DIAGBUFF         Point to Diag 0C buffer
         DC    X'8333000C'         Diagnose x'0C'
*
         L     R5,=A(IYEAR)
         USING IYEAR,R5
*
         PACK  PKTBCD,DIAGBUFF+6(2)  Convert year
         CVB   R4,PKTBCD
         ST    R4,IYEAR
         MH    R4,=H'100'            YY00
         PACK  PKTBCD,DIAGBUFF(2)    Convert Month
         CVB   R3,PKTBCD
         ST    R3,IMONTH
         AR    R4,R3
         MH    R4,=H'100'            YYMM00
         PACK  PKTBCD,DIAGBUFF+3(2)  Convert date
         CVB   R3,PKTBCD
         ST    R3,IDATE
         AR    R4,R3
         ST    R4,0(R1)              YYMMDD
*
         PACK  PKTBCD,DIAGBUFF+8(2)  Convert Hours
         CVB   R4,PKTBCD
         ST    R4,IHOUR
         MH    R4,=H'100'            HH00
         PACK  PKTBCD,DIAGBUFF+11(2) Convert Minutes
         CVB   R3,PKTBCD
         ST    R3,IMINUTE
         AR    R4,R3
         ST    R4,0(R2)              HHMM
         PACK  PKTBCD,DIAGBUFF+14(2) Convert Seconds
         CVB   R3,PKTBCD
         ST    R3,ISECOND            Only common for seconds
*
RETURN   L     R13,4(,R13)
         LM    R14,R12,12(R13)
         OI    12(R13),X'FF'
         BR    R14
*
         DS    0D
DIAGBUFF DC    4CL8' '
PKTBCD   DC    XL8'00'
*
         LTORG
*
SLATE    COM
IYEAR    DS    F
IMONTH   DS    F
IDATE    DS    F
IHOUR    DS    F
IMINUTE  DS    F
ISECOND  DS    F
         DS    34F
         REGEQU
         END
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
