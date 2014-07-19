*
* $Id: sndsx.s,v 1.1.1.1 1996/02/15 17:47:39 mclareni Exp $
*
* $Log: sndsx.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:39  mclareni
* Kernlib
*
*
         TITLE 'SNDSX / SXSND'
***********************************************************************
*                                                                     *
*                              S N D S X                              *
*                              =========                              *
*                                                                     *
***********************************************************************
*                                                                     *
* FUNCTION:     CONVERT NORD-100 32-BIT NORMALISED FLOATING POINT     *
*               NUMBERS TO IBM 32-BIT FLOATING POINT REPRESENTATION.  *
*               NUMBERS WHOSE MAGNITUDE EXCEEDS THE IBM MAXIMUM       *
*               WILL HAVE A MAGNITUDE OF X'7FFFFFFF' AFTER CONVERSION.*
*                                                                     *
* CALLING SEQUENCE:     CALL SNDSX(WORDS,NWORDS)                      *
*                                                                     *
* ARGUMENTS:     WORDS  - AN ARRAY OF FULLWORDS CONTAINING THE        *
*                         NORD-100 32-BIT FLOATING POINT NUMBERS      *
*                         WHICH ARE TO BE CONVERTED.  THE CONVERTED   *
*                         NUMBERS WILL REPLACE THE ORIGINAL ONES IN   *
*                         THIS ARRAY.                                 *
*                                                                     *
*                NWORDS - AN INTEGER CONSTANT OR 4-BYTE INTEGER       *
*                         VARIABLE CONTAINING THE NUMBER OF WORDS TO  *
*                         BE CONVERTED.  A VALUE LESS THAN ONE WILL   *
*                         CAUSE CONTROL TO BE RETURNED TO THE CALLER  *
*                         WITH NO PROCESSING.                         *
*                                                                     *
***********************************************************************
         EJECT
***********************************************************************
*                                                                     *
*                              S X S N D                              *
*                              =========                              *
*                                                                     *
***********************************************************************
*                                                                     *
* FUNCTION:     CONVERT IBM 32-BIT NORMALISED FLOATING POINT NUMBERS  *
*               TO NORD-100 32-BIT FLOATING POINT REPRESENTATION.     *
*               NUMBERS WHOSE MAGNITUDE IS LESS THAN THE NORD MINIMUM *
*               WILL HAVE A MAGNITUDE OF X'00000001' AFTER CONVERSION.*
*                                                                     *
* CALLING SEQUENCE:     CALL SXSND(WORDS,NWORDS)                      *
*                                                                     *
* ARGUMENTS:     WORDS  - AN ARRAY OF FULLWORDS CONTAINING THE        *
*                         IBM 32-BIT FLOATING POINT NUMBERS WHICH ARE *
*                         TO BE CONVERTED.  THE CONVERTED NUMBERS     *
*                         WILL REPLACE THE ORIGINAL ONES IN THIS      *
*                         ARRAY.                                      *
*                                                                     *
*                NWORDS - AN INTEGER CONSTANT OR 4-BYTE INTEGER       *
*                         VARIABLE CONTAINING THE NUMBER OF WORDS TO  *
*                         BE CONVERTED.  A VALUE LESS THAN ONE WILL   *
*                         CAUSE CONTROL TO BE RETURNED TO THE CALLER  *
*                         WITH NO PROCESSING.                         *
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
SNDSX    CSECT
#if defined(CERNLIB_QMIBMXA)
SNDSX    AMODE ANY
SNDSX    RMODE ANY
#endif
         SPACE 2
*  P R O L O G U E
         SPACE
         USING SNDSX,R15
         B     SAVEREGS
         DC    AL1(7),CL7'SNDSX'     IDENTIFIER
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
         LM    R5,R6,MASKS    R5 := X'00400000', R6 := '01000000'
         LE    F6,MAXIMUM     F6 := MAXIMUM CONVERTIBLE NUMBER
         SPACE
LOOP1    EQU   *
         LE    F0,0(0,R9)     F0 := NUMBER TO BE CONVERTED
         LPER  F0,F0          F0 := ABSOLUTE VALUE OF THE NUMBER
         CER   F0,F6          IS IT MORE THAN THE MAXIMUM CONVERTIBLE ?
         BNH   CONVERT1       NO, GO AND CONVERT IT
         OC    0(4,R9),=X'7FFFFFFFF' YES, STORE MAXIMUM, PRESERVE SIGN
         B     LOOP1END
CONVERT1 L     R2,0(0,R9)     R2 := NUMBER TO BE CONVERTED
         LTR   R3,R2          R3 := NUMBER TO BE CONVERTED
         BZ    LOOP1END       BRANCH IF ZERO, (NO CONVERSION NECESSARY)
         LA    R4,0(0,R2)     R4 := BITS 8-31 OF NUMBER TO BE CONVERTED
         SLR   R2,R4          R2 := IBM CHARACTERISTIC IN BITS 0-7
         OR    R3,R5          R3 := 23-BIT MANTISSA IN BITS 9-31
         SLL   R3,1           R3 := 24-BIT MANTISSA IN BITS 8-31
         LA    R3,0(0,R3)     ZERO BITS 0-7
         SRA   R4,22          R4 := RESIDUAL POWER 2 EXPONENT
         BZ    COMBINE1       BRANCH IF IT IS ZERO
         SLL   R3,0(R4)       PRENORMALISE
         SRL   R3,4           POSTNORMALISE
         ALR   R2,R6          INCREMENT THE CHARACTERISTIC
COMBINE1 OR    R2,R3          COMBINE THE CHARACTERISTIC AND MANTISSA
         ST    R2,0(0,R9)     STORE THE RESULT
LOOP1END BXLE  R9,R10,LOOP1   GO AND PROCESS THE NEXT NUMBER
         SPACE
         B     EPILOGUE
         EJECT
*  E N T R Y   P O I N T   S X S N D
         SPACE
         ENTRY SXSND
         USING SXSND,R15
         CNOP  0,4
SXSND    B     EP0102
         DC    AL1(7),CL7'SXSND'     IDENTIFIER
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
         L     R5,MASKS       R5 := X'00400000'
         LE    F6,MINIMUM     F6 := MINIMUM CONVERTIBLE NUMBER
         SPACE
LOOP2    EQU   *
         LE    F0,0(0,R9)     F0 := NUMBER TO BE CONVERTED
         LPER  F0,F0          F0 := ABSOLUTE VALUE OF THE NUMBER
         BZ    LOOP2END       BRANCH IF THE FRACTION IS ZERO
         CER   F0,F6          IS IT LESS THAN THE MINIMUM CONVERTIBLE ?
         BNL   CONVERT2       NO, GO AND CONVERT IT
         NC    0(4,R9),=X'80000000' YES, PRESERVE THE SIGN
         MVI   3(R9),X'01'    STORE THE MINIMUM
         B     LOOP2END
CONVERT2 L     R4,0(0,R9)     R4 := NUMBER TO BE CONVERTED
         LA    R3,0(0,R4)     R3 := 24-BIT MANTISSA IN BITS 8-31
         SLR   R4,R3          R4 := CHARACTERISTIC IN BITS 0-7
         SR    R2,R2          R2 := 0
         SLDL  R2,9           R2 := MOST SIGNIFICANT BIT OF MANTISSA
NORMAL2  LTR   R2,R2          IS IT 1 ?
         BNZ   COMBINE2       YES, BRANCH
         SLDL  R2,1           R2 := MOST SIGNIFICANT BIT OF MANTISSA
         SLR   R4,R5          DECREMENT THE CHARACTERISTIC
         B     NORMAL2        GO AND TEST THE MOST SIGNIFICANT BIT
COMBINE2 SRL   R3,10          R3 := 22-BIT MANTISSA IN BITS 10-31
         OR    R4,R3          COMBINE THE CHARACTERISTIC AND MANTISSA
         ST    R4,0(0,R9)     STORE THE RESULT
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
         SLA   R10,2          R10 := 4*(NWORDS-1),  EXCESS -> OVERFLOW
         LA    R11,0(R9,R10)  R11 -> LAST NUMBER TO BE CONVERTED
         LA    R10,4          R10 := 4 FOR LOOP INCREMENT
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
         DS    0F
MAXIMUM  DC    X'7F3FFFFF'    LARGEST CONVERTIBLE NORD 32-BIT NUMBER
MINIMUM  DC    X'00800002'    SMALLEST CONVERTIBLE IBM 32-BIT NUMBER
MASKS    DC    X'00400000',X'01000000'
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
