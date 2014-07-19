*
* $Id: w16movib.s,v 1.1.1.1 1996/03/08 15:21:51 mclareni Exp $
*
* $Log: w16movib.s,v $
* Revision 1.1.1.1  1996/03/08 15:21:51  mclareni
* Epio
*
*
*  IBM ASSEMBLER CODE
#if defined(CERNLIB_IBM)
         TITLE 'W16MOV'
         PRINT NOGEN
W16MOV   CSECT
#if defined(CERNLIB_QMIBMXA)
W16MOV   AMODE ANY
W16MOV   RMODE ANY
#endif
***********************************************************************
*                                                                     *
*                             W 1 6 M O V                             *
*                             ===========                             *
*                                                                     *
***********************************************************************
*                                                                     *
* FUNCTION:     MOVE HALFWORDS                                        *
*                                                                     *
* CALLING SEQUENCE:     CALL W16MOV(SOURCE,N1,TARGET,N2,N3)           *
*                                                                     *
* ARGUMENTS:     SOURCE - SOURCE OF HALFWORDS TO BE MOVED             *
*                N1     - FIRST HALFWORD TO BE MOVED FROM SOURCE      *
*                TARGET - TARGET FOR THE MOVED HALFWORDS              *
*                N2     - FIRST HALFWORD TO BE USED IN TARGET         *
*                N3     - NUMBER OF HALFWORDS TO BE MOVED             *
*                                                                     *
***********************************************************************
         EJECT
***********************************************************************
*                                                                     *
*                   M O D I F I C A T I O N   L O G                   *
*                   ===============================                   *
*                                                                     *
***********************************************************************
*                                                                     *
*                               1 . 0 0                               *
*                               =======                               *
*                                                                     *
* ORIGINAL VERSION.                                                   *
*                                                                     *
* R. MATTHEWS, CERN/DD, SEPTEMBER 1983.                               *
*                                                                     *
***********************************************************************
         EJECT
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
         EJECT
*  P R O L O G U E
         SPACE
         USING W16MOV,R15
         B     SAVEREGS
         DC    AL1(7),CL7'W16MOV'    IDENTIFIER
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
         SPACE 2
         LM    R2,R6,0(R1)    R2 -> SOURCE;  R3 -> STARTING LOCATION
*                             R4 -> TARGET;  R5 -> STARTING LOCATION
*                             R6 -> NUMBER OF HALFWORDS TO MOVE
         L     R6,0(0,R6)     R6 := NUMBER OF HALFWORDS TO MOVE
         LTR   R6,R6          IS IT GREATER THAN ZERO ?
         BNP   EPILOGUE       NO, BRANCH
         L     R3,0(0,R3)     R3 := STARTING LOCATION IN SOURCE
         BCTR  R3,0           SUBTRACT 1
         SLL   R3,1           MULTIPLY BY 2
         AR    R2,R3          R2 -> STARTING LOCATION IN SOURCE
         L     R5,0(0,R5)     R5 := STARTING LOCATION IN TARGET
         BCTR  R5,0           SUBTRACT 1
         SLL   R5,1           MULTIPLY BY 2
         AR    R4,R5          R4 -> STARTING LOCATION IN SOURCE
         SLL   R6,1           R6 := NUMBER OF BYTES TO MOVE
         LR    R3,R6          R3 := NUMBER OF BYTES TO MOVE
         LR    R5,R6          R5 := NUMBER OF BYTES TO MOVE
         MVCL  R4,R2          PERFORM THE MOVE
         SPACE 2
*  E P I L O G U E
         SPACE
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
         SPACE 2
         END
#endif
