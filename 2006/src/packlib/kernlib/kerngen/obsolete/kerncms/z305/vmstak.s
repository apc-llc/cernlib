*
* $Id: vmstak.s,v 1.1.1.1 1996/02/15 17:51:55 mclareni Exp $
*
* $Log: vmstak.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:55  mclareni
* Kernlib
*
*
VMSTAK   CSECT
#if defined(CERNLIB_QMIBMXA)
VMSTAK   AMODE ANY
VMSTAK   RMODE ANY
#endif
*
* CERN PROGLIB# Z305    VMSTAK          .VERSION KERNCMS  1.05  900713
*   J.D. Shiers, 9-JUL-1990, BASED ON Z305 VMPACK routines
*
* Mods        Date    Comments
* J.Shiers  90/07/12  for CMS 4 - use ATTN function instead
* I.Reguero 90/11/19  for CMS 4 - use SVC 202 instead of CMSCALL
*
*     TITLE 'VMSTAK  WRITE LINE OF UP TO 255 CHARS TO PROGRAM STACK'
*
*     CALL FROM FORTRAN 77 IS:
*     CALL VMSTAK(STRING,OPT,IRC)
*
*     STRING (INPUT)    CHARACTER*(*) THE COMMAND
*     OPT    (INPUT)    CHARACTER*1   'L' (LIFO) OR 'F' (FIFO)
*     IRC    (OUTPUT)   RETURN CODE
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
         USING VMSTAK,R15
         B     SAVEREGS
         DC    AL1(7),CL7'VMSTAK'    IDENTIFIER
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
*
         LR    R11,R1        R11 -> ARGUMENT LIST
*
*  E N S U R E   T H E   O P E R A T I N G   S Y S T E M   I S   C M S
*
         L     R2,16         R2 -> CVT
         TM    116(R2),X'40' IS IT CMS ?
         BNO   ERROR1        NO, BRANCH
*
*  G E T   T H E   C O M M A N D   A N D   I T S   L E N G T H
*
         LR    R2,R11        R2 -> ARGUMENT LIST
         S     R2,=F'4'      R2 -> ARGUMENT LIST PREFIX
         L     R2,0(0,R2)    R2 := ARGUMENT LIST PREFIX
         L     R0,VJZLINIT   R0 := ADDRESS OF JZLINIT#
         LTR   R0,R0         IS IT PRESENT ?
         BNZ   FUJITSU       YES, BRANCH
*
         AR    R2,R11        R2 -> SECONDARY ARGUMENT LIST
         L     R2,0(0,R2)    R2 -> COMMAND LENGTH
         L     R5,0(0,R2)    R5 := COMMAND LENGTH
         B     DOCMD
*
FUJITSU  LH    R5,0(0,R2)    R5 := COMMAND LENGTH
*
DOCMD    LM    R2,R4,0(R1)   R2->COMMAND,R3->IFLAG,R4->IRC
         L     R3,0(,R3)     R3 = IFLAG
         CLI   R3,C'L'       LIFO?
         BNE   PUTSTAK
#if defined(CERNLIB_QMIBMXA)
         SR    R3,R3         FIFO
*              Need to store FIFO or LIFO in PLIST
#endif
#if defined(CERNLIB_QCMS4)
         MVI   PLIST+8,C'F'
*
#endif
#if defined(CERNLIB_QMIBMXA)
PUTSTAK  CMSSTACK TEXT=((R2),(R5)),ORDER=(R3)
*              Version for CMS 4 - no CMSSTACK MACRO
#endif
#if defined(CERNLIB_QCMS4)
PUTSTAK  ST    R2,PLIST+12   Store address of line in PLIST
         STC   R5,PLIST+12   Store length
         LA    R1,PLIST      Address of PLIST
*        CMSCALL PLIST=(R1)  Here we go...
         SVC   202
         DC    AL4(1)
#endif
         ST    R15,0(,R4)    Store return code
         B     EPILOGUE
*
*  E R R O R   P R O C E S S I N G
*
ERROR1   WTO   MF=(E,WTOLIST1)
         ABEND 999,DUMP
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
#if defined(CERNLIB_QCMS4)
PLIST    DS    0D
         DC    CL8'ATTN'
         DC    CL4'LIFO'
         DS    1D            length + address of line
#endif
WTOLIST1 WTO   'CRNVM999 VMCMS: THE OPERATING SYSTEM IS NOT CMS',      &
               ROUTCDE=(11),DESC=(6),MF=L
*
         LTORG
*
         NUCON
*
         END
