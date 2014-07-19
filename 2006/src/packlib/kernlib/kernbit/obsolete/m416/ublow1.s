*
* $Id: ublow1.s,v 1.1.1.1 1996/02/15 17:47:40 mclareni Exp $
*
* $Log: ublow1.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:40  mclareni
* Kernlib
*
*
#if defined(CERNLIB_CDC)
          IDENT UBLOW1
          ENTRY    UBLOW1
*         SUBROUTINE UBLOW1 (A,X,NBITS)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
*
*                              X1 LOOP COUNT          B1 LAST STORE ADR
*                              X2                     B2 STORE ADR
*                              X3                     B3 NBITS REMAINING
*                              X5 WORD BEING ANALYS.  B5
*                              X6 RESULT              B6 = 60
*                              X7 =-1                 B7 =-1
*
          VFD  36/0HUBLOW1,24/0
 UBLOW1   BSS  1
          SA2  A1+2            .X2=ADDRESS OF NBITS
          MX7  59              .X7=-1
          SB7  X7              .B7=-1
          SA3  X2              .X3=NBITS
          SB6  60
          SA4  A1-B7           .X4=ADDRESS OF X
          SA5  X1              .ADR(A)
          SB3  X3              .NBITS
          SB2  X4
          SB1  X4+B3
 LOOP1    SX1  B3              .J=NBITS
          GE   B6,B3,LOOP2     .J=60 IF NBITS.GT.60
          SX1  B6
 LOOP2    BX6  -X7*X5          .PICK BIT
          IX1  X1+X7           .J=J-1
          LX5  59              .RIGHT-SHIFT OF ANALYSED WORD BY 1
          SA6  B2              .STORE RESULT
          SB2  B2-B7           .STEP STORE ADR
          NZ   X1,LOOP2
          GE   B2,B1,UBLOW1
          SA5  A5-B7           .NEXT WORD TO BE BLOWN UP
          SB3  B3-B6           .NBITS= NBITS - 60
          EQ   LOOP1
          END
#endif
#if defined(CERNLIB_IBM)
         TITLE 'UBLOW1'
***********************************************************************
*                                                                     *
*                             U B L O W 1                             *
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
UBLOW1   CSECT
#endif
#if defined(CERNLIB_QMIBMXA)
UBLOW1   AMODE ANY
UBLOW1   RMODE ANY
#endif
#if defined(CERNLIB_IBM)
         SPACE 2
*  P R O L O G U E
         SPACE
         USING UBLOW1,R15
         B     SAVEREGS
         DC    AL1(7),CL7'UBLOW1'    IDENTIFIER
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
         LM    R2,R4,0(R1)    R2 -> A;  R3 -> K;  R4 -> N
         L     R4,0(0,R4)     R4 := N
         L     R8,=F'-4'      R8 := -4 FOR LOOP INCREMENT
         AR    R3,R8          R3 -> K(0)
         SRDL  R4,5           R4 := NUMBER OF FULLWORDS TO PROCESS
         SRL   R5,27          R5 := NUMBER OF ADDITIONAL BITS
         LTR   R4,R4          ARE THERE ANY FULLWORDS ?
         BNP   EXTRABIT       NO, BRANCH
         SPACE
NEXTWORD L     R7,0(0,R2)     R7 := WORD TO BE DISPERSED
         LR    R9,R3          R9 := TEST VALUE FOR LOOPING
         LA    R3,32*4(0,R3)  R3 -> FIRST TARGET WORD
         SPACE
NEXTBIT  SR    R6,R6          R6 := 0
         SLDL  R6,1           SHIFT IN THE NEXT BIT
         ST    R6,0(0,R3)     STORE IT IN K
         BXH   R3,R8,NEXTBIT  LOOP TO PROCESS THE NEXT BIT OF THE WORD
         SPACE
         LA    R2,4(0,R2)     R2 -> NEXT WORD TO BE DISPERSED
         LA    R3,32*4(0,R3)  R3 -> NEXT TARGET AREA
         BCT   R4,NEXTWORD    LOOP TO PROCESS THE NEXT FULLWORD
         SPACE
EXTRABIT LTR   R5,R5          ARE THERE ANY ADDITIONAL BITS ?
         BNP   EPILOGUE       NO, BRANCH
         L     R6,0(0,R2)     GET THEM INTO R6
         SRDL  R6,0(R5)       SHIFT THEM INTO R7
         LR    R9,R3          R9 := TEST VALUE FOR LOOPING
         SLL   R5,2           R5 := NUMBER OF TARGET BYTES
         LA    R3,0(R5,R3)    R3 -> FIRST TARGET WORD
         SPACE
NEXTBIT1 SR    R6,R6          R6 := 0
         SLDL  R6,1           SHIFT IN THE NEXT BIT
         ST    R6,0(0,R3)     STORE IT IN K
         BXH   R3,R8,NEXTBIT1 LOOP TO PROCESS THE NEXT BIT
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
