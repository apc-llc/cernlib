*
* $Id: vmsatn.s,v 1.1.1.1 1996/02/15 17:51:55 mclareni Exp $
*
* $Log: vmsatn.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:55  mclareni
* Kernlib
*
*
VMSATN   CSECT
*
* CERN PROGLIB# Z305    VMSATN/VMTATN   .VERSION KERNIBM  2.19  861204
* ORIG. 01/01/86  RM
*
*        REFER TO CERN PROGRAM LIBRARY MEMBER Z305 FOR DETAILS
*
*                   M O D I F I C A T I O N   L O G
*                   ===============================
*
*     1 . 0 0
*
*
* ORIGINAL VERSION.
*
* R. MATTHEWS, CERN/DD, JANUARY 1986.
*
*********************************************************************
*
#if defined(CERNLIB_QMIBMXA)
        SPLEVEL  SET=2
VMSATN   AMODE ANY
VMSATN   RMODE ANY
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
         PRINT NOGEN
*
*  P R O L O G U E
*
         USING VMSATN,R15
         B     SAVEREGS
         DC    AL1(7),CL7'VMSATN'    IDENTIFIER
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
* D E T E R M I N E   I F   A N   E X I T   A L R E A D Y   E X I S T S
*
         USING NUCON,0
         L     R2,0(0,R1)     R2 -> FIRST ARGUMENT
         L     R3,TAXEADDR    R3 := EXIT ADDRESS
         LTR   R3,R3          IS THERE ALREADY AN EXIT ADDRESS ?
         BZ    STAX           NO, BRANCH
         ST    R3,0(0,R2)     USE IT AS A NON-ZERO RETURN CODE
         B     EPILOGUE
*
* E S T A B L I S H   A N   A T T E N T I O N   E X I T
*
STAX     STAX  EXIT           ISSUE THE STAX MACRO
         SRA   R15,2                 DIVIDE THE RETURN CODE BY 4
         ST    R15,0(0,R2)    STORE IT FOR THE CALLER
         B     EPILOGUE
*
* A T T E N T I O N   E X I T
*
EXIT     EQU   *
         USING EXIT,R15
         MVI   FLAG+3,1       SWITCH ON THE ATTENTION FLAG
         BR    R14            RETURN TO THE SYSTEM
         DROP  R15
*
*
*********************************************************************
*
*                             V M T A T N
*                             ===========
*
*********************************************************************
*
         ENTRY VMTATN
         USING VMTATN,R15
VMTATN   EQU   *
         L     R1,0(0,R1)     R1 -> FIRST ARGUMENT
         L     R0,FLAG        R0 := ATTENTION FLAG
         ST    R0,0(0,R1)     STORE IT FOR THE CALLER
         MVI   FLAG+3,0       SWITCH OFF THE ATTENTION FLAG
         BR    R14            RETURN TO THE CALLER
         DROP  R15
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
*  D A T A   A R E A S
*
FLAG     DC    F'0'
*
         LTORG
*
         NUCON
*
         END
