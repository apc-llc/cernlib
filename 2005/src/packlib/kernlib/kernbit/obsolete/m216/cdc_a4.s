*
* $Id: cdc_a4.s,v 1.1.1.1 1996/02/15 17:47:37 mclareni Exp $
*
* $Log: cdc_a4.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:37  mclareni
* Kernlib
*
*
CDC$A4   CSECT
#if defined(CERNLIB_QMIBMXA)
CDC$A4   AMODE ANY
CDC$A4   RMODE ANY
#endif
*
*        CALL CDC$A4(CDCBUF,IBMBUF,LEN,BYTWD,FILL)
*
* CDCBUF: Array of 64-bit words containig a 60-bit CDC word
*         right justified (output of READWI)
* IBMBUF: Any memory space of at least LEN*BYTWD bytes
* LEN   : Number of CDC words to decode
* BYTWD : Number of characters taken from each CDC word (Optional,
*         defaults to 4
* FILL  : Optional value to be stored in CDCBUF (padded to 64 bits on
*         the left) to avoid Error 14 from CONCDC
*
* Note: a CDC word contains 10 6-bit CDC characters; they are transla-
* ted from CDC Display 6-bit code to EBCDIC 8-bit code, and assembled
* contiguously into IBMBUF (whose word boundaries are ignored).  Only
* if BYTWD=4  the CDC and IBM word counts are equal, with the loss of
* of the 6 rightmost CDC characters (normally not used if the program
* was written for 32-bit machines).
* If BYTWD > 10 blanks are added to the right of the 10 characters of
* each CDC word.
                                  SPACE
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
               PRINT NOGEN
               USING *,R15
         B     START
         DC    X'07',CL7'CDC$A4'
SAVEAREA DS    3F
                                  SPACE
START    STM   R14,R12,12(R13)
         LR    R12,R13
         LA    R13,SAVEAREA
         ST    R13,8(R12)
         ST    R12,4(R13)
               USING SAVEAREA,R13
               DROP  R15
         LM    R2,R6,0(R1)        Get addr of args
         XR    R8,R8              For filler
         LTR   R4,R4              Length last parm?
         BM    DEFBYTSW           Yes, set default BYTWD & fill
         LTR   R5,R5              BYTWD last parm?
         BNM   BYTESWD            No, all parms given - do work
         B     DEFFILL            Yes, set default filler
DEFBYTSW LA    R5,=F'4'           Our default for BYTWD
DEFFILL  XR    R6,R6              ...and clear Filler address
BYTESWD  L     R4,0(R4)           Get length of array
LOOP     LM    R10,R11,0(R2) <--+ Get a word (odd)
         SLDL  R10,2            | Shift left R10-R11 2 bits
         L     R7,0(R5)         | Get Bytes per Word in R7 (loop count)
LOOPC    STCM  R10,MSB,0(R3) <+ | Store in output array (8 bits)
         NI    0(R3),X'3F'    | | Clear leftmost 2 bits
         TR    0(1,R3),CDCTB  | | Translate CDC Display Code to EBCDIC
         LA    R3,1(R3)       | | Point to next byte (output)
         SLDL  R10,6          | | Shift-in next CDC character or blank
         BCT   R7,LOOPC ------+ | Loop for next CDC character
         LTR   R6,R6            | Filler required?
         BZ    NOFILL           | No
         L     R9,0(R6)         | Yes, get it
         STM   R8,R9,0(R2)      | Store it in place (input)
NOFILL   LA    R2,8(R2)         | Point to next CDC word (input)
         BCT   R4,LOOP ---------+ Loop for next CDC word
END      L     R13,4(R13)
**       MVI   12(R13),X'FF'
         LM    R2,R12,28(R13)
         BR    R14
                             SPACE 3
*                0123456701234567    octal
*
CDCTB    DC    C' ABCDEFGHIJKLMNO'   00-17
         DC    C'PQRSTUVWXYZ01234'   20-37
         DC    C'56789+-*/()$= ,.'   40-57
         DC    C'#[]%"_!&&''?<>@\^;' 60-77
*
MSB      EQU   B'1000'       Most significant byte (leftmost)
         LTORG
         END
