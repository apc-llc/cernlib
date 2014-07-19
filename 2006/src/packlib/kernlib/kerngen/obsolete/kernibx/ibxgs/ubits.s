*
* $Id: ubits.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: ubits.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
UBITS    CSECT
*
* CERN PROGLIB# M503    UBITS           .VERSION KERNIBX  1.01  900524
* ORIG.  OCT 81,  R.MATTHEWS, M.METCALF, CERN/DD
* Modified for AIX, Roger Howard, January 1990
*
*     CALL UBITS (STRING,LENGTH,LOCS,NUMBER)
*
* FUNCTION:   EXAMINE A STRING OF BITS AND STORE THE LOCATIONS OF
*             THE BITS HAVING THE VALUE 1 IN SUCCESSIVE WORDS OF
*             AN ARRAY.
*
* ARGUMENTS:  STRING - A FULLWORD ARRAY CONTAINING THE BIT STRING
*
*             LENGTH - THE NUMBER OF BITS IN THE BIT STRING
*
*             LOCS   - AN INTEGER*4 ARRAY TO RECEIVE THE LOCATIONS
*                      OF THE BITS WHOSE VALUE IS 1
*
*             NUMBER - THE NUMBER OF BITS FOUND TO HAVE THE VALUE 1
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
*  P R O L O G U E
*
         USING *,R15
         ENTRY _ubits_
_ubits_  STM   R2,R1,16(R13)
         LR    R9,R0          AIX: address or arg1 = STRING
         LR    R11,R2         AIX: address of arg3 = LOCS
         LR    R12,R3         AIX: address of arg4 = NUMBER
         L     R3,0(,R1)      AIX: value of arg2 = LENGTH
         SR    R5,R5          R5 := 0  (INDEX FOR LOCS)
         SR    R4,R4          R4 := 0  (LOOP COUNTER)
         LA    R2,1           R2 := 1  (LOOP INCREMENT)
         LR    R8,R2          R8 := 1
LOOP     BXH   R4,R2,ALLDONE
         BCT   R8,TESTBIT     BRANCH UNLESS CURRENT WORD IS EXHAUSTED
NEWORD   L     R6,0(0,R9)     R6 := NEW WORD OF STRING
         LA    R9,4(0,R9)     R9 -> NEXT WORD OF STRING
         LA    R8,32          PRIME THE COUNTER
TESTBIT  SRDL  R6,1           SHIFT LEAST SIGNIFICANT BIT OF R6 INTO R7
         LTR   R7,R7          IS IT 1 ?
         BNM   TESTWORD       NO, BRANCH
         ST    R4,0(R5,R11)   STORE ITS LOCATION IN LOCS
         LA    R5,4(0,R5)     INCREMENT THE INDEX FOR LOCS
TESTWORD LTR   R6,R6          ARE THERE ANY MORE 1 BITS IN THE WORD ?
         BNZ   LOOP           YES, BRANCH TO PROCESS THEM
         AR    R4,R8          OTHERWISE INCREMENT THE LOOP COUNTER BY
         SR    R4,R2          THE NUMBER OF BITS REMAINING UNTESTED
         BXLE  R4,R2,NEWORD   AND TEST WHETHER FINISHED
ALLDONE  SRA   R5,2           NUMBER = (R5)/4
         ST    R5,0(0,R12)    STORE NUMBER
EPILOGUE EQU   *
         LM    R2,R12,16(R13)
         BR    R14
         END
#ifdef CERNLIB_TCGEN_UBITS
#undef CERNLIB_TCGEN_UBITS
#endif
