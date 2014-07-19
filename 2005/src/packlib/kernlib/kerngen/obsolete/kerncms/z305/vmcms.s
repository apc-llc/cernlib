*
* $Id: vmcms.s,v 1.1.1.1 1996/02/15 17:51:54 mclareni Exp $
*
* $Log: vmcms.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:54  mclareni
* Kernlib
*
*
VMCMS    CSECT
#if defined(CERNLIB_QMIBMXA)
VMCMS    AMODE ANY
VMCMS    RMODE ANY
#endif
*
* CERN PROGLIB# Z305    VMCMS           .VERSION KERNIBM  2.17  860131
* ORIG. 01/12/85  RM
*
*        REFER TO CERN PROGRAM LIBRARY MEMBER Z305 FOR DETAILS
*
*                   M O D I F I C A T I O N   L O G
*                   ===============================
*
*
*     2 . 0 1
*
*    Adapted for XA: uses the documented SCAN macro and then
*    CMSCALL to issue the command.
*
*    14th April 1989  Tony Cass  CERN/DD
*
*     2 . 0 0
*
* THIS VERSION USES A DIRECT INTERFACE TO CMS VIA SVC 202 WITH AN R1
* HIGH-ORDER BYTE VALUE OF X'01', ('ADDRESS COMMAND'), IN PLACE OF
* THE PREVIOUSLY USED SUBCOM INTERFACE. THIS TECHNIQUE ENABLES THE
* USE OF IOS3270, WHICH USES THE VALUE X'01' TO DETERMINE WHETHER
* EXECOMM IS AVAILABLE. THE 'ADDRESS COMMAND' FLAG REQUIRES THAT EXEC
* NAMES AND CP COMMANDS ARE PREFIXED WITH 'EXEC' AND 'CP' RESPECTIVELY
* WHEN THEY ARE INVOKED, AS IN REXX EXECS.
* THE SYSTEM COMMAND SCAN ROUTINE IS CALLED TO BUILD THE PLIST AND
* EXTENDED PLIST. THIS REQUIRES THE USE OF DMSKEY TO CHANGE THE PSW
* KEY.
* R. MATTHEWS, CERN/DD, JANUARY 1986.
*
*     1 . 0 1
*
* DETECTION OF JZLINIT# USED TO DETERMINE COMPILER ENVIRONMENT TO
* ENSURE CORRECT OPERATION IN VS FORTRAN LOAD MODE.
* R. MATTHEWS, CERN/DD, NOVEMBER 1985.
*
*     1 . 0 0
*
* ORIGINAL VERSION.
*
* R. MATTHEWS, CERN/DD, JULY 1985.
*
*********************************************************************
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
*
         PRINT NOGEN
*
*  P R O L O G U E
*
         USING VMCMS,R15
         B     SAVEREGS
         DC    AL1(7),CL7'VMCMS'     IDENTIFIER
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
*
*  E N S U R E   T H E   O P E R A T I N G   S Y S T E M   I S   C M S
*
         L     R2,16          R2 -> CVT
         TM    116(R2),X'40'  IS IT CMS ?
         BNO   ERROR1         NO, BRANCH
*
*  G E T   T H E   C O M M A N D   A N D   I T S   L E N G T H
*
         LR    R2,R11         R2 -> ARGUMENT LIST
         S     R2,=F'4'       R2 -> ARGUMENT LIST PREFIX
         L     R2,0(0,R2)     R2 := ARGUMENT LIST PREFIX
         L     R0,VJZLINIT    R0 := ADDRESS OF JZLINIT#
         LTR   R0,R0          IS IT PRESENT ?
         BNZ   FUJITSU        YES, BRANCH
*
         AR    R2,R11         R2 -> SECONDARY ARGUMENT LIST
         L     R2,0(0,R2)     R2 -> COMMAND LENGTH
         L     R0,0(0,R2)     R0 := COMMAND LENGTH
         B     DOCMD
*
FUJITSU  LH    R0,0(0,R2)     R0 := COMMAND LENGTH
*
*  B U I L D   T H E   P L I S T   A N D   T H E   E P L I S T
*
DOCMD    EQU   *
#if !defined(CERNLIB_QMIBMXA)
         L     R1,0(0,R11)    R1 -> COMMAND
         USING NUCON,0
         DMSKEY NUCLEUS
         L     R15,ASCANN
         BALR  R14,R15
         DMSKEY RESET
*
*  I S S U E   T H E   C O M M A N D
*
         LA    R0,NUCPLIST    R0 -> EXTENDED PLIST
         ICM   R1,B'1000',=X'01'  INSERT THE 'ADDRESS COMMAND' FLAG
         SVC   202            ISSUE THE COMMAND
         DC    AL4(1)
#endif
#if defined(CERNLIB_QMIBMXA)
         L     R7,0(,R11)
         LR    R8,R0
         SCAN  TEXT=((R7),(R8)),BUFFER=(SCANBUFF,512)
         CMSCALL PLIST=(R1),EPLIST=(R0)
#endif
         L     R2,4(0,R11)    R2 -> SECOND ARGUMENT
         ST    R15,0(0,R2)    STORE THE RETURN CODE
         B     EPILOGUE
*
*  E R R O R   P R O C E S S I N G
*
ERROR1   WTO   MF=(E,WTOLIST1)
         ABEND 999,DUMP
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         L     R14,12(0,R13)
         LM    R2,R12,28(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
*  D A T A   A R E A S
*
         WXTRN JZLINIT#
VJZLINIT DC    A(JZLINIT#)
*
WTOLIST1 WTO   'CRNVM999 VMCMS: THE OPERATING SYSTEM IS NOT CMS',      &
               ROUTCDE=(11),DESC=(6),MF=L
*
#if defined(CERNLIB_QMIBMXA)
SCANBUFF DS    128F
*
#endif
         LTORG
*
         NUCON
*
         END
