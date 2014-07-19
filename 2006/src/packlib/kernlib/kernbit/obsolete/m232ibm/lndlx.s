*
* $Id: lndlx.s,v 1.1.1.1 1996/02/15 17:47:40 mclareni Exp $
*
* $Log: lndlx.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:40  mclareni
* Kernlib
*
*
         TITLE 'LNDLX/LXLND'
***********************************************************************
*                                                                     *
*                              L N D L X                              *
*                              =========                              *
*                                                                     *
***********************************************************************
*                                                                     *
* FUNCTION:     CONVERT NORD-100 48-BIT NORMALISED FLOATING POINT     *
*               NUMBERS TO IBM 64-BIT FLOATING POINT REPRESENTATION.  *
*               NUMBERS WHOSE MAGNITUDE EXCEEDS THE IBM MAXIMUM WILL  *
*               HAVE A MAGNITUDE OF X'7FFFFFFFFFFFFFFF' AFTER         *
*               CONVERSION.  NUMBERS WHOSE MAGNITUDE IS LESS THAN THE *
*               IBM MINIMUM WILL HAVE A MAGNITUDE OF                  *
*               X'0010000000000000' AFTER CONVERSION.                 *
*                                                                     *
* CALLING SEQUENCE:     CALL LNDLX(DWORDS,NWORDS)                     *
*                                                                     *
* ARGUMENTS:     DWORDS - AN ARRAY OF DOUBLEWORDS CONTAINING THE      *
*                         NORD-100 48-BIT FLOATING POINT NUMBERS      *
*                         WHICH ARE TO BE CONVERTED.  THE CONVERTED   *
*                         NUMBERS WILL REPLACE THE ORIGINAL ONES IN   *
*                         THIS ARRAY.  EACH 48-BIT NORD NUMBER MUST   *
*                         BE RIGHT JUSTIFIED IN A DOUBLEWORD.         *
*                                                                     *
*                NWORDS - AN INTEGER CONSTANT OR 4-BYTE INTEGER       *
*                         VARIABLE CONTAINING THE NUMBER OF DOUBLE    *
*                         WORDS TO BE CONVERTED.  A VALUE LESS THAN   *
*                         ONE WILL CAUSE CONTROL TO BE RETURNED TO    *
*                         THE CALLER WITH NO PROCESSING.              *
*                                                                     *
***********************************************************************
         EJECT
***********************************************************************
*                                                                     *
*                              L X L N D                              *
*                              =========                              *
*                                                                     *
***********************************************************************
*                                                                     *
* FUNCTION:     CONVERT IBM 64-BIT NORMALISED FLOATING POINT NUMBERS  *
*               TO NORD-100 48-BIT FLOATING POINT REPRESENTATION.     *
*                                                                     *
* CALLING SEQUENCE:     CALL LXLND(DWORDS,NWORDS)                     *
*                                                                     *
* ARGUMENTS:     DWORDS - AN ARRAY OF DOUBLEWORDS CONTAINING THE      *
*                         IBM 64-BIT BIT FLOATING POINT NUMBERS       *
*                         WHICH ARE TO BE CONVERTED.  THE CONVERTED   *
*                         NUMBERS WILL REPLACE THE ORIGINAL ONES IN   *
*                         THIS ARRAY, EACH WILL BE RIGHT JUSTIFIED IN *
*                         A DOUBLEWORD WITH ZERO FILL.                *
*                                                                     *
*                NWORDS - AN INTEGER CONSTANT OR 4-BYTE INTEGER       *
*                         VARIABLE CONTAINING THE NUMBER OF DOUBLE    *
*                         WORDS TO BE CONVERTED.  A VALUE LESS THAN   *
*                         ONE WILL CAUSE CONTROL TO BE RETURNED TO    *
*                         THE CALLER WITH NO PROCESSING.              *
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
* R. MATTHEWS, CERN/DD, SEPTEMBER 1980.                               *
*                                                                     *
***********************************************************************
         EJECT
         EJECT
         PRINT NOGEN
LNDLX    CSECT
#if defined(CERNLIB_QMIBMXA)
LNDLX    AMODE ANY
LNDLX    RMODE ANY
#endif
         SPACE 2
*  P R O L O G U E
         SPACE
         USING LNDLX,R15
         B     SAVEREGS
         DC    AL1(7),CL7'LNDLX'     IDENTIFIER
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
         BAL   R12,SETUP
         SPACE
         LM    R7,R8,MASKS1   R7 := X'0007FFFF',  R8 := X'00080000'
         SPACE
LOOP1    EQU   *
         LM    R2,R3,0(R9)    R2,3 := NUMBER TO BE CONVERTED
         SLDL  R2,16          LEFT JUSTIFY IT
         LTR   R5,R2          R5 := BITS 0-31 OF THE NUMBER
         BZ    LOOP1END       BRANCH IF ZERO, (NO CONVERSION NECESSARY)
         ST    R2,0(0,R9)     STORE THE SIGN LEFT JUSTIFIED
         SLDL  R4,1           SAVE THE SIGN BIT IN R4
         CL    R5,=X'81F9FFFF' IS THE NUMBER TOO LARGE ?
         BNH   NOTHIGH        NO, BRANCH
         OC    0(8,R9),=X'7FFFFFFFFFFFFFFF'  STORE MAX, PRESERVE SIGN
         B     LOOP1END
NOTHIGH  CL    R5,=X'7DFB0000' IS THE NUMBER TOO SMALL ?
         BNL   CONVERT1       NO, BRANCH
         NC    0(8,R9),=X'8000000000000000' YES, PRESERVE THE SIGN
         MVI   1(R9),X'10'    STORE THE MINIMUM
         B     LOOP1END
CONVERT1 SLDL  R2,8           R2,3 := 56-BIT MANTISSA IN BITS 8-63
         LA    R2,0(0,R2)     ZERO BITS 0-7
         LR    R6,R5          R6 := 15-BIT CHARACTERISTIC IN BITS 0-14
         NR    R6,R7          ZERO BITS 0-12
         SLR   R5,R6          R5 := 13-BIT POWER 16 CHARACTERISTIC
         SRA   R6,17          R6 := RESIDUAL POWER 2 EXPONENT
         BZ    COMBINE1       BRANCH IF IT IS ZERO
         SLDL  R2,0(R6)       PRENORMALISE
         SRDL  R2,4           POSTNORMALISE
         ALR   R5,R8          INCREMENT THE CHARACTERISTIC
COMBINE1 SLDL  R4,1           SAVE THE BIAS IN R4
         SLL   R5,6           R5 := 6-BIT POWER 16 EXPONENT IN BITS 0-5
         SRDL  R4,2           REPLACE SIGN; R5 := IBM CHARACTERISTIC
         OR    R2,R5          COMBINE THE CHARACTERISTIC AND MANTISSA
         STM   R2,R3,0(R9)    STORE THE RESULT
LOOP1END BXLE  R9,R10,LOOP1   GO AND PROCESS THE NEXT NUMBER
         SPACE
         B     EPILOGUE
         EJECT
*  E N T R Y   P O I N T   L X L N D
         SPACE
         ENTRY LXLND
         USING LXLND,R15
         CNOP  0,4
LXLND    B     EP0102
         DC    AL1(7),CL7'LXLND'     IDENTIFIER
EP0101   DC    A(SAVEAREA)
EP0102   STM   R14,R12,12(R13)
         LR    R4,R13
         L     R13,EP0101
         ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
         DROP  R15
         SPACE 2
         BAL   R12,SETUP
         SPACE
         LM    R6,R8,MASKS2   R6:=7E000000, R7:=80000000, R8:=00010000
         SPACE
LOOP2    LM    R4,R5,0(R9)    R4,5 := NUMBER TO BE CONVERTED
         LTR   R3,R4          R3 := BITS 0-31 OF THE NUMBER
         BZ    LOOP2END       BRANCH IF ZERO, (NO CONVERSION NECESSARY)
         LA    R4,0(0,R4)     R4,5 := 56-BIT MANTISSA IN BITS 8-63
         SLR   R3,R4          R3 := IBM CHARACTERISTIC
         SLDL  R2,1           SAVE THE SIGN IN R2
         SRA   R3,6           EXPAND CHARACTERISTIC WITH 6 BIAS BITS
         XR    R3,R6          COMPLEMENT THE 6 NEW BITS
         SRDL  R2,1           REPLACE SIGN; R3 := NORD CHARACTERISTIC
         SLDL  R4,8           R4 := 56-BIT MANTISSA IN BITS 0-55
NORMAL2  CLR   R4,R7          IS IT NORMALISED ?
         BNL   COMBINE2       YES, BRANCH
         SLDL  R4,1           NO, SHIFT IT LEFT BY ONE BIT
         SLR   R3,R8          DECREMENT THE CHARACTERISTIC
         B     NORMAL2        GO AND TEST FOR NORMALISATION
COMBINE2 SRL   R3,16          R3 := 16-BIT CHARACTERISTIC IN BITS 16-31
         STM   R3,R4,0(R9)    STORE THE RESULT
LOOP2END BXLE  R9,R10,LOOP2   GO AND PROCESS THE NEXT NUMBER
         SPACE
         B     EPILOGUE
         EJECT
*  P R E - C O N V E R S I O N   I N I T I A L I S A T I O N
         SPACE
SETUP    LM    R9,R10,0(R1)   R9 -> WORDS;  R10 -> NWORDS
         L     R10,0(0,R10)   R10 := NWORDS
         LTR   R10,R10        ARE THERE ANY NUMBERS TO BE CONVERTED ?
         BNP   EPILOGUE       NO, BRANCH
         SPACE
         BCTR  R10,0          R10 := NWORDS-1
         SLA   R10,3          R10 := 8*(NWORDS-1),  EXCESS -> OVERFLOW
         LA    R11,0(R9,R10)  R11 -> LAST NUMBER TO BE CONVERTED
         LA    R10,8          R10 := 8 FOR LOOP INCREMENT
         SPACE
         BR    R12
         EJECT
*  E P I L O G U E
         SPACE
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
**       OI    12(R13),X'FF'
         BR    R14
         EJECT
*  D A T A   A R E A S
         SPACE
         DS    0D
MASKS1   DC    X'0007FFFF',X'00080000'
MASKS2   DC    X'7E000000',X'80000000',X'00010000'
         SPACE 2
         LTORG
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
         SPACE 2
F0       EQU   0
F2       EQU   2
F4       EQU   4
F6       EQU   6
         SPACE 2
         END
