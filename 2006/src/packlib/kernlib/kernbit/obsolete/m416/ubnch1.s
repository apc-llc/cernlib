*
* $Id: ubnch1.s,v 1.1.1.1 1996/02/15 17:47:40 mclareni Exp $
*
* $Log: ubnch1.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:40  mclareni
* Kernlib
*
*
#if defined(CERNLIB_CDC)
          IDENT   UBNCH1
          ENTRY   UBNCH1
*         SUBROUTINE UBNCH1 (A,X,NBITS)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
*
*                              X1                     B1 LAST TAKE ADR +
*                              X2                     B2 STORE ADR
*                              X3 NBITS REMAINING     B3
*                              X4 = 60                B4 STOP INNER LOOP
*                              X5 A(J)                B5 SHIFT COUNTER
*                              X6 COMPILE RESULT      B6 = 60
*                              X7 =-1                 B7 =-1
*
          VFD  36/0HUBNCH1,24/0
 UBNCH1   BSS  1
          SA2  A1+2            .X2=ADDRESS OF NBITS
          MX7  59              .X7=-1
          SB7  X7              .B7=-1
          SA3  X2              .NBITS
          SB6  60
          SB1  X1
          SA2  A1-B7           .X2=ADDRESS OF X
          SB1  B1+X3           .ADR( A(NBITS+1)  )
          SA5  X1              .ADR(A)
          SX4  B6
 LOOP1    SB4  X3              .N=NBITS
          MX6  0
          SB5  B0              .SHIFT ZERO FOR FIRST WORD
          GE   B6,B4,LOOP2     .N=60 IF NBITS.GT.60
          SB4  B6
 LOOP2    BX1  -X7*X5          .MASK A(J)
          SA5  A5-B7           .A(J+1)
          LX1  B5,X1           .SHIFT MASKED A(J)
          SB5  B5-B7           .BUMP SHIFT COUNTER
          BX6  X6+X1           .COMPILE RESULT
          LT   B5,B4,LOOP2
          SA6  X2              .STORE 1 WORD OF RESULTS
          IX3  X3-X4           .NBITS= NBITS - 60
          SB5  A5
          GE   B5,B1,UBNCH1
          IX2  X2-X7           .BUMP STORE ADR
          EQ   LOOP1
          END
#endif
#if defined(CERNLIB_IBM)
         TITLE 'UBNCH1'
***********************************************************************
*                                                                     *
*                             U B N C H 1                             *
*                             ===========                             *
*                                                                     *
***********************************************************************
*                                                                     *
*       REFER TO CERN PROGRAM LIBRARY MEMBER M416 FOR DETAILS.        *
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
* R. MATTHEWS, CERN/DD, JULY 1982.                                    *
*                                                                     *
***********************************************************************
         EJECT
         EJECT
         PRINT NOGEN
UBNCH1   CSECT
#endif
#if defined(CERNLIB_QMIBMXA)
UBNCH1   AMODE ANY
UBNCH1   RMODE ANY
#endif
#if defined(CERNLIB_IBM)
         SPACE 2
*  P R O L O G U E
         SPACE
         USING UBNCH1,R15
         B     SAVEREGS
         DC    AL1(7),CL7'UBNCH1'    IDENTIFIER
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
*  P R O C E S S
         SPACE
         LM    R2,R4,0(R1)    R2 -> K;  R3 -> A;  R4 -> N
         L     R4,0(0,R4)     R4 := N
         L     R8,=F'-4'      R8 := -4 FOR LOOP INCREMENT
         AR    R2,R8          R2 -> K(0)
         SRDL  R4,5           R4 := NUMBER OF FULLWORDS TO PROCESS
         SRL   R5,27          R5 := NUMBER OF ADDITIONAL BITS
         LTR   R4,R4          ARE THERE ANY FULLWORDS ?
         BNP   EXTRABIT       NO, BRANCH
         SPACE
NEXTWORD LR    R9,R2          R9 := TEST VALUE FOR LOOPING
         LA    R2,32*4(0,R2)  R2 -> FIRST SOURCE WORD
         SPACE
NEXTBIT  SLL   R6,1           MAKE SPACE FOR THE NEXT BIT
         O     R6,0(0,R2)     OR IN THE NEXT BIT
         BXH   R2,R8,NEXTBIT  LOOP TO PROCESS THE NEXT BIT OF THE WORD
         SPACE
         ST    R6,0(0,R3)     STORE THE ACCUMULATED BITS IN A
         LA    R3,4(0,R3)     R3 -> NEXT TARGET WORD
         LA    R2,32*4(0,R2)  R2 -> NEXT SOURCE AREA
         BCT   R4,NEXTWORD    LOOP TO PROCESS THE NEXT FULLWORD
         SPACE
EXTRABIT LTR   R5,R5          ARE THERE ANY ADDITIONAL BITS ?
         BNP   EPILOGUE       NO, BRANCH
         LR    R9,R2          R9 := TEST VALUE FOR LOOPING
         SLL   R5,2           R5 := NUMBER OF SOURCE BYTES
         LA    R2,0(R5,R2)    R2 -> FIRST SOURCE WORD
         SR    R6,R6          R6 := 0
         SPACE
NEXTBIT1 SLL   R6,1           MAKE SPACE FOR THE NEXT BIT
         O     R6,0(0,R2)     OR IN THE NEXT BIT
         BXH   R2,R8,NEXTBIT1 LOOP TO PROCESS THE NEXT BIT
         SPACE
         ST    R6,0(0,R3)     STORE THE ACCUMULATED BITS IN A
         SPACE 2
*  E P I L O G U E
         SPACE
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
**       MVI   12(R13),X'FF'
         BR    R14
         SPACE
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
         SPACE
         END
#endif
