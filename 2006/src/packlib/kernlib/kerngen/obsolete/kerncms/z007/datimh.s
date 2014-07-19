*
* $Id: datimh.s,v 1.1.1.1 1996/02/15 17:51:47 mclareni Exp $
*
* $Log: datimh.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:47  mclareni
* Kernlib
*
*
         PRINT NOGEN
DATIMH   CSECT
#if defined(CERNLIB_QMIBMXA)
DATIMH   AMODE ANY
DATIMH   RMODE ANY
#endif
***********************************************************************
*                                                                     *
* CERN Program Library Z007 for VM/CMS                                *
*                                                                     *
*       CHARACTER*8 CDAY,CTIME                                        *
*       CALL DATIMH(IDAY,ITIME)                                       *
*                                                                     *
*       CDAY  returned as DD/MM/YY                                    *
*       CTIME          as HH.MM.SS                                    *
*                                                                     *
***********************************************************************
*                                                                     *
* Modification Log                                                    *
*                                                                     *
* Original:  Tony Cass  CERN/DD  March 1989                           *
*                                                                     *
***********************************************************************
         USING DATIMH,R15
         B     SAVEREGS
         DC    AL1(7),CL7'DATIMH' Name
         DC    CL4'1.00'          Version.modification
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
         MVC   0(3,R1),DIAGBUFF+3  DD section of date
         MVC   3(3,R1),DIAGBUFF    MM section of date
         MVC   6(2,R1),DIAGBUFF+6  YY section of date
         MVC   0(8,R2),DIAGBUFF+8  Time with ':' separator
         MVI   2(R2),C'.'          Add period
         MVI   5(R2),C'.'           ditto
*
RETURN   L     R13,4(,R13)
         LM    R14,R12,12(R13)
         OI    12(R13),X'FF'
         BR    R14
*
         DS    0D
DIAGBUFF DC    4CL8' '
         REGEQU
         END
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
