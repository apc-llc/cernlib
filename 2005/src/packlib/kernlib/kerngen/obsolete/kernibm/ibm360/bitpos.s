*
* $Id: bitpos.s,v 1.1.1.1 1996/02/15 17:53:10 mclareni Exp $
*
* $Log: bitpos.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:10  mclareni
* Kernlib
*
*
BITPOS   CSECT
*
* CERN PROGLIB# M508    BITPOS          .VERSION KERNIBM  2.20  870601
* ORIG.  OCT 81,  R.MATTHEWS
*
*     CALL BITPOS (STRING,LENGTH,LOCS,NUMBER)
*
* FUNCTION:   EXAMINE A STRING OF BITS AND STORE THE LOCATIONS OF
*             THE BITS HAVING THE VALUE 1 IN SUCCESSIVE WORDS OF
*             AN ARRAY.
*
* ARGUMENTS:  STRING - AN ARRAY OF FULLWORDS CONTAINING THE BIT STRING
*
*             LENGTH - THE NUMBER OF BITS IN THE BIT STRING
*
*             LOCS   - AN INTEGER*4 ARRAY TO RECEIVE THE LOCATIONS
*                      OF THE BITS WHOSE VALUE IS 1
*
*             NUMBER - THE NUMBER OF BITS FOUND TO HAVE THE VALUE 1
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
BITPOS   AMODE ANY
BITPOS   RMODE ANY
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
         USING BITPOS,R15
         B     SAVEREGS
         DC    AL1(7),CL7'BITPOS'    IDENTIFIER
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
         LM    R9,R12,0(R1)   R9  -> STRING,  R10 -> LENGTH,
*                             R11 -> LOCS,  R12 -> NUMBER
         LA    R2,1           R2 := 1  (LOOP INCREMENT)
         L     R3,0(0,R10)    R3 := LENGTH  (LOOP COMPARAND)
         SR    R3,R2
         SR    R4,R4          R4 := 0  (LOOP COUNTER)
         SR    R5,R5          R5 := 0  (INDEX FOR LOCS)
*
NEWORD   L     R6,0(0,R9)     R6 := NEW WORD OF STRING
         LA    R9,4(0,R9)     R9 -> NEXT WORD OF STRING
         LA    R8,32          PRIME THE COUNTER
TESTBIT  SR    R8,R2          R8 := NUMBER OF BITS UNTESTED IN WORD
         SRDL  R6,1           SHIFT LEAST SIGNIFICANT BIT OF R6 INTO R7
         LTR   R7,R7          IS IT 1 ?
         BNM   TESTWORD       NO, BRANCH
         ST    R4,0(R5,R11)   STORE ITS LOCATION IN LOCS
         LA    R5,4(0,R5)     INCREMENT THE INDEX FOR LOCS
TESTWORD LTR   R6,R6          ARE THERE ANY MORE 1 BITS IN THE WORD ?
         BNZ   LOOPEND        YES, BRANCH TO PROCESS THEM
         AR    R4,R8          OTHERWISE INCREMENT THE LOOP COUNTER BY
         BXLE  R4,R2,NEWORD   THE NUMBER OF BITS REMAINING UNTESTED
LOOPEND  BXLE  R4,R2,TESTBIT  AND TEST WHETHER FINISHED
*
         SRA   R5,2           NUMBER = (R5)/4
         ST    R5,0(0,R12)    STORE NUMBER
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
         END
#ifdef CERNLIB_TCGEN_BITPOS
#undef CERNLIB_TCGEN_BITPOS
#endif
