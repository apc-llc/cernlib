*
* $Id: setbytml.s,v 1.1.1.1 1996/02/15 17:47:44 mclareni Exp $
*
* $Log: setbytml.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:44  mclareni
* Kernlib
*
*
#if (defined(CERNLIB_IBM)||defined(CERNLIB_CDC))&&(!defined(CERNLIB_FORTRAN))
#if defined(CERNLIB_CDC)
          IDENT  SETBYT
          ENTRY  SETBYT
          EXT    SHSERR
*
*  CALL SETBYT(ADDR,IBEG,ILEN,IRES)
*
*  REVISED OCT 28 1980 BY T LINDELOF
SETBYT    BSS    1
          SA2    A1+1
          SA2    X2                .A2=IBEG ADDRESS
          SA4    MAX               .X4=MAX=2**47-1
          IX4    X4-X2
          NG     X4,ZERO           .IF IBEG > MAX ERROR EXIT
          SA4    =1                .X4=1
          IX2    X2-X4             .X2=IBEG-1
          NG     X2,ZERO           .IF IBEG < 1 ERROR EXIT
*
*                B-REGISTER ALLOCATION
*                 B3-ILEN
*                 B4-SHIFT PARAMETER
*                 B5-MOD(IBEG-1,60)
*                 B6-60
*                 B7-TEMP
*
          SA3    A1+2              .A3=ADDRESS OF ILEN ADDRESS
          SA3    X3                .X3=ILEN
          SB3    X3                .B3=ILEN
          PL     X3,INST           .IF ILEN < 1 ERROR EXIT
*
***************************************************
*
*                ERROR HANDLING
*
ZERO      SA2    A1+1
          SA3    A1+2
          LX6    X3
          LX7    X2
          SA7    AIBEG
          SA6    AILEN
          SA1    AIBEG
          RJ     SHSERR
          ZR     SETBYT
AIBEG     BSS    1
AILEN     BSS    1
*
****************************************************
*
INST      SA4    =60               .X4=60
          SB6    X4                .B6=60
          IX4    X4-X3             .X4=60-ILEN
          NG     X4,ZERO           .IF ILEN > 60 ERROR EXIT
*
*                OBTAIN ADDRESS MODIFICATOR
*
          LX3    X2                .X3=X2=IBEG-1
          SX4    B6                .X4=60
          PX3    B0,X3             .PACK IBEG
          PX4    B0,X4             .PACK 60
          NX4    X4                .NORMALIZE 60
          NX3    X3                .NORMALIZE IBEG
          FX3    X3/X4             .X3=(IBEG-1)/60
          UX0    B1,X3             .UNPACK RESULT
          LX3    B1,X0             .TAKE INTEGER PART
*
*                X3 CONTAINS MODIFICATOR OF BEGIN ADDRESS
*
          IX7    X1+X3             .X7=ADDRESS OF FIRST TARGET WORD
          SX4    B6                .X4=60
          IX3    X3*X4
          SX5    X2                .X5=IBEG-1
          IX5    X5-X3
          SB5    X5                .X5=B5=MOD(IBEG-1,60)
*
*                NOW X5+1 POINTS TO FIRST BIT TO BE SET
*
          SB7    B5+B3
          SB7    B6-B7
          SX3    B7                .-X3=#BITS AFFECTED IN TARGET WD 2
*
*                LEFT BITS OF IRES MUST BE CLEARED
*
          SA2    A1+3              .X2=ADDRESS OF IRES
          SA2    X2                .X2=IRES
          SB7    B3-1              .B7=ILEN-1
          MX4    1
          AX4    B7
          LX4    B3                .X4=ILEN 1-BITS RIGHT-ADJUSTED
          BX2    X2*X4             .X2=ILEN BITS OF DATA, RIGHT-ADJUSTED
*
*                COMPUTATION OF SHIFT PARAMETER
*
*                AT THIS POINT REGISTERS CONTAIN THE FOLLOWING DATA
*                X2 THE ILEN RIGHT-MOST BITS OF IRES
*                X3 NEGATIVE IF TWO TARGET WORDS WILL BE AFFECTED
*                X4 ILEN 1-BITS, RIGHT-ADJUSTED
*                B3 ILEN
*                B5 MOD(IBEG-1,60)
*                B6 60
*
          SB4    B6-B3
          SB4    B4-B5
          GE     B4,POS
          SB4    B4+B6
*                                  .B4=MOD(120-ILEN-MOD(IBEG-1,60),60)
 POS      LX2    B4
          LX4    B4
*
*   X2 (DATA) AND X4 (MASK) NOW SHIFTED TO CORRECT POSITION
*   BUT (IF X3>0) END-AROUND SO THAT RIGHT-MOST BITS GO TO (X7) AND
*   LEFT-MOST BITS TO (X7+1)
*
*                STORE THE FIRST PART OF DATA
*
          BX5    X2
          BX6    -X4
          PL     X3,ONE
          BX1    -X3
          SB7    X1-1
          MX1    1
          AX1    B7
          BX5    -X1*X5            .CLEAR LEFT-MOST BITS OF DATA
          BX6    X1+X6             .AND OF THE INVERTED MASK
 ONE      SA1    X7                .X1=TARGET WORD 1
          BX6    X1*X6             .CLEAR BITS TO BE SET
          BX6    X5+X6             .X6=OR BETWEEN TARGET AND DATA
          SA6    A1                .STORE RESULT
*
*                STORE THE SECOND PART OF DATA
*
          PL     X3,GOUT
          MX6    1
          AX6    B7
          BX5    X2*X6
          SA1    X7+1              .X1=TARGET WORD 2
          BX6    -X6*X1            .CLEAR BITS TO BE SET
          BX6    X5+X6             .X6=OR BETWEEN TARGET AND DATA
          SA6    A1                .STORE SECOND WORD
*
GOUT      ZR     SETBYT            .RETURN TO CALLER
*
MAX       DATA   00003777777777777777B
*
          END
#endif
#if defined(CERNLIB_IBM)
         TITLE 'SETBYT'
***********************************************************************
*                                                                     *
*                             S E T B Y T                             *
*                             ===========                             *
*                                                                     *
* FUNCTION:     A USER-SUPPLIED BIT-STRING IS COPIED TO A SPECIFIED   *
*               LOCATION IN AN ARRAY                                  *
*                                                                     *
* CALLING SEQUENCE:     CALL SETBYT(ARRAY,LOCATN,NBITS,STRING)        *
*                                                                     *
* ARGUMENTS:     ARRAY  - THE TARGET ARRAY INTO WHICH THE BIT-STRING  *
*                         IS TO BE COPIED.  THIS MAY BE OF ANY TYPE   *
*                         AND LENGTH CONSISTENT WITH THE LENGTH OF THE*
*                         BIT-STRING AND THE LOCATION TO WHICH IT IS  *
*                         TO BE COPIED.                               *
*                LOCATN - A 4-BYTE INTEGER CONSTANT OR INTEGER        *
*                         VARIABLE WHICH SPECIFIES THE LOCATION TO    *
*                         WHICH THE BIT-STRING IS TO BE COPIED IN     *
*                         THE TARGET ARRAY.  BITS IN THE TARGET ARRAY *
*                         ARE NUMBERED FROM THE LEFT STARTING WITH    *
*                         BIT 1 AND THE NUMBERING CONTINUES ACROSS    *
*                         ARRAY ELEMENT BOUNDARIES.  THE BIT-STRING   *
*                         IS COPIED SO THAT ITS LEFT-MOST BIT         *
*                         OCCUPIES THE BIT LOCATION SPECIFIED BY      *
*                         LOCATN AND MAY SPAN ELEMENTS OF THE TARGET  *
*                         ARRAY.                                      *
*                NBITS  - A 4-BYTE INTEGER CONSTANT OR INTEGER        *
*                         VARIABLE WHICH SPECIFIES THE NUMBER OF      *
*                         BITS IN THE BIT STRING.  IT MAY HAVE ANY    *
*                         VALUE IN THE RANGE 1-32.                    *
*                BITS   - A 4-BYTE AREA CONTAINING THE BIT STRING     *
*                         WHICH IS TO BE COPIED.  THE RIGHT-MOST      *
*                         NBITS BITS ARE COPIED FROM THIS AREA INTO   *
*                         THE TARGET ARRAY.                           *
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
*                               2 . 0 0                               *
*                               =======                               *
*                                                                     *
* NEW VERSION.                                                        *
*                                                                     *
* R.MATTHEWS, NOVEMBER 1979.                                          *
*                                                                     *
***********************************************************************
         EJECT
         EJECT
         PRINT NOGEN
SETBYT   CSECT
#endif
#if defined(CERNLIB_QMIBMXA)
SETBYT   AMODE ANY
SETBYT   RMODE ANY
#endif
#if defined(CERNLIB_IBM)
         SPACE 2
*  P R O L O G U E
         SPACE
         USING SETBYT,R15
         B     SAVEREGS
         DC    AL1(7),CL7'SETBYT'    IDENTIFIER
         DC    C'2.00'               VERSION.MODIFICATION LEVEL
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
         SPACE
         LR    R11,R1         R11 -> ARGUMENT LIST
         SPACE
         LM    R2,R5,0(R1)    R2 -> ARRAY;  R3 -> LOCATN;
*                             R4 -> NBITS;  R5 -> STRING
         SPACE 2
* T E S T   T H E   V A L U E S   O F   L O C A T N   A N D   N B I T S
         SPACE
         L     R4,0(0,R4)     R4 := NBITS
         LTR   R4,R4          IS NBITS .GT.0 ?
         BNP   ERROR1         NO, BRANCH
         C     R4,=F'32'      IS NBITS .GT. 32 ?
         BH    ERROR1         YES, BRANCH
         SPACE
         L     R6,0(0,R3)     R6 := LOCATN
         LTR   R6,R6          IS LOCATN .GT. 0 ?
         BNP   ERROR1         NO, BRANCH
         SPACE 2
* F I N D   T A R G E T   B Y T E   &   B I T   A N D   T H E   S P A N
         SPACE
         BCTR  R6,0           R6 := LOCATN-1  (I.E. BIT ZERO IS ORIGIN)
         SRDL  R6,3           R6 := BYTE LOCATION
         SRL   R7,29          R7 := BIT LOCATION
         AR    R2,R6          R2 -> TARGET BYTE IN ARRAY
         LR    R3,R7          R3 := BIT LOCATION IN TARGET BYTE
         SPACE
         LA    R8,0(R3,R4)
         BCTR  R8,0
         SRL   R8,3           R8 := NUMBER OF BYTES FOR AND & OR OPS.
         SPACE 2
*  P U T   Z E R O S   I N   T A R G E T   A R E A   O F   A R R A Y
         SPACE
         L     R6,ONEBITS     R6 := 11111111111111111111111111111111
         SR    R7,R7          R7 := 00000000000000000000000000000000
         SRDL  R6,0(R4)       SHIFT NBITS INTO R7
         SLDL  R6,32          SHIFT THEM TO THE TOP OF R6;  R7 := 0
         SRDL  R6,0(R3)       SHIFT THEM TO THE TARGET BIT LOCATION
         STM   R6,R7,DOUBLE   STORE THEM
         MVC   MASK,ONEBITS   MASK := X'FFFFFFFFFFFFFFFF'
         XC    MASK,DOUBLE   MASK HAS 0'S IN TARGET AREA, 1'S ELSEWHERE
         EX    R8,NC1         AND THE MASK WITH THE TARGET ARRAY
         SPACE 2
*  P U T   B I T   S T R I N G   I N   T H E   T A R G E T   A R E A
         SPACE
         MVC   DOUBLE(4),0(R5)  MOVE USER'S BIT-STRING INTO FULLWORD
         L     R6,DOUBLE      R6 := USER'S BIT STRING
         SR    R7,R7          R7 := 00000000000000000000000000000000
         SRDL  R6,0(R4)       SHIFT REQUIRED BITS INTO R7
         SLDL  R6,32          SHIFT THEM TO THE TOP OF R6;  R7 := 0
         SRDL  R6,0(R3)       SHIFT THEM TO THE TARGET BIT LOCATION
         STM   R6,R7,MASK     STORE THEM
         EX    R8,OC1         OR THEM WITH THE TARGET ARRAY
         B     EPILOGUE
         SPACE 2
NC1      NC    0(0,R2),MASK   AND MASK WITH THE TARGET AREA OF ARRAY
         SPACE 2
OC1      OC    0(0,R2),MASK   OR MASK WITH THE TARGET AREA OF ARRAY
         EJECT
*  E R R O R   C O N D I T I O N S
         SPACE
ERROR1   EQU   *
         LM    R2,R3,4(R1)    R2 -> LOCATN;  R3 -> NBITS
         CALL  SHSERR,((R2),(R3)),VL
         B     EPILOGUE
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
DOUBLE   DS    D
MASK     DS    D
ONEBITS  DC    X'FFFFFFFFFFFFFFFF'
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
         END
#endif
#endif
