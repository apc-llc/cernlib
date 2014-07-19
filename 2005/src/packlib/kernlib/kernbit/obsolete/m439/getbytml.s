*
* $Id: getbytml.s,v 1.1.1.1 1996/02/15 17:47:43 mclareni Exp $
*
* $Log: getbytml.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:43  mclareni
* Kernlib
*
*
#if (defined(CERNLIB_IBM)||defined(CERNLIB_CDC))&&(!defined(CERNLIB_FORTRAN))
#if defined(CERNLIB_CDC)
          IDENT  GETBYT
          ENTRY  GETBYT
          EXT    SHRERR
*CALLING
*  CALL GETBYT(ADDR,NBEG,LBIT,IRES)
GETBYT    BSS    1
          SA2    A1+1
          SA2    X2                .X2=NBEG
          SA4    MAX               .X4=MAX=140,737,488,355,327=2**47-1
          IX4    X4-X2
          NG     X4,ZERO           .IF NBEG > MAX GO TO ZERO
          SA4    =1                .X4=1
          IX2    X2-X4             .NBEG=NBEG-1
          NG     X2,ZERO           .IF NBEG<1 GO TO ZERO
*
          SA3    A1+2
          SA3    X3                .X3=LBIT
          SB3    X3                ,B3=LBIT
          PL     X3,INST           .IF LBIT > 0 GO TO INST
***************************************************
*
*                ERROR HANDLING
*
*
ZERO      SA2    A1+1
          SA3    A1+2
          LX6    X3
          LX7    X2
          SA7    ANBEG
          SA6    ALBIT
          SA1    ANBEG
          RJ     SHRERR
          ZR     RET
ANBEG     BSS    1
ALBIT     BSS    1
*
****************************************************
*
INST      SA4    =60               .X4=60
          SB6    X4                .B6=60
          IX4    X4-X3             .
          NG     X4,ZERO           .IF LBIT > 60 GO TO ZERO
*
*                WE MUST COMPUTE VALUE (NBEG/60) IN ORDER TO
*                OBTAIN ADDRESS MODIFICATOR.
*
          LX3    X2                .X3=NBEG
          SX4    B6                .X4=60
          PX3    B0,X3             PACK NBEG
          PX4    B0,X4             PACK 60
          NX4    X4                NORMALIZE 60
          NX3    X3                NORMALIZE NBEG
          FX3    X3/X4             X3=NBEG/60
          UX0    B1,X3             UNPACK RESULT
          LX3    B1,X0             .TAKE INTEGER PART
*
*                X3 CONTAINS MODIFICATOR OF BEGIN ADDRESS
*
          IX7    X1+X3
*
          SX4    B6                .X4=60
          IX3    X3*X4
          IX5    X2-X3             .X5=R(NBEG/60)
*
*                NOW X5 CONTAINS BIT NUMBER IN WORD
*
          SB5    X5                .B5=X5
          SA4    MASK+B5           .X4=MASK TO BE NEEDED
          SA2    X7                .X2=ADDR
          BX2    X2*X4             .X2=X2 .AND. X4
*
*                COMPUTATION OF SHIFT PARAMETER
*
          SB4    B6-B3             .B4=60-LBIT
          SB4    B4-B5             .B4=60-LBIT-R(NBEG/60)
          ZR     B4,RET            .GO OUT IF SHIFT=0
          GT     B4,RIGHT           .IF B4>0 GO TO RIGHT
*
*                LEFT SHIFT
*
          SB4    -B4               .CHANGE SIGN
          LX2    B4                .SHIFT <--- BY B4
          SA3    A2+1              .READ NEXT WORD
          SB5    B4-B6             .B5=60-SHIFT PARAMETER
          LX3    B5                .SHIFT ---> BY B5
          SB5    -B5
          SA4    B5+MASK
          BX3    X3*X4
          BX2    X3+X2             . LOGICAL OR
          ZR     RET               .GO OUT
*
*
*
RIGHT     SB4    -B4
          LX2    B4                .SHIFT X2 ---> BY B4
          SB4    -B4
          SA4    B4+MASK
          BX2    X2*X4
          ZR     RET               .GO OUT
*
*
RET       LX6    X2                .X6=X2
          SA5    A1+3              .A5=ADDRESS OF RESULT ADDRESS
          SA6    X5                .STORE THE RESULT
          ZR     GETBYT            .RETURN TO CALLER
MASK      DATA   77777777777777777777B
          DATA   37777777777777777777B
          DATA   17777777777777777777B
          DATA   07777777777777777777B
          DATA   03777777777777777777B
          DATA   01777777777777777777B
          DATA   00777777777777777777B
          DATA   00377777777777777777B
          DATA   00177777777777777777B
          DATA   00077777777777777777B
          DATA   00037777777777777777B
          DATA   00017777777777777777B
          DATA   00007777777777777777B
          DATA   00003777777777777777B
          DATA   00001777777777777777B
          DATA   00000777777777777777B
          DATA   00000377777777777777B
          DATA   00000177777777777777B
          DATA   00000077777777777777B
          DATA   00000037777777777777B
          DATA   00000017777777777777B
          DATA   00000007777777777777B
          DATA   00000003777777777777B
          DATA   00000001777777777777B
          DATA   00000000777777777777B
          DATA   00000000377777777777B
          DATA   00000000177777777777B
          DATA   00000000077777777777B
          DATA   00000000037777777777B
          DATA   00000000017777777777B
          DATA   00000000007777777777B
          DATA   00000000003777777777B
          DATA   00000000001777777777B
          DATA   00000000000777777777B
          DATA   00000000000377777777B
          DATA   00000000000177777777B
          DATA   00000000000077777777B
          DATA   00000000000037777777B
          DATA   00000000000017777777B
          DATA   00000000000007777777B
          DATA   00000000000003777777B
          DATA   00000000000001777777B
          DATA   00000000000000777777B
          DATA   00000000000000377777B
          DATA   00000000000000177777B
          DATA   00000000000000077777B
          DATA   00000000000000037777B
          DATA   00000000000000017777B
          DATA   00000000000000007777B
          DATA   00000000000000003777B
          DATA   00000000000000001777B
          DATA   00000000000000000777B
          DATA   00000000000000000377B
          DATA   00000000000000000177B
          DATA   00000000000000000077B
          DATA   00000000000000000037B
          DATA   00000000000000000017B
          DATA   00000000000000000007B
          DATA   00000000000000000003B
          DATA   00000000000000000001B
          DATA   00000000000000000000B
*
MAX       DATA   00003777777777777777B
*
          END
#endif
#if defined(CERNLIB_IBM)
GETBYT   CSECT
#endif
#if defined(CERNLIB_QMIBMXA)
GETBYT   AMODE ANY
GETBYT   RMODE ANY
#endif
#if defined(CERNLIB_IBM)
* CALL GETBYT(ADR,NBEG,LBIT,IRES)
* ADR - ADDRESS OF BIT STRING
* NBEG - NUMBER OF BEGIN BIT IN ADR
* LBIT - LENGTH IN BITS
* IRES _ RESULT
***********************************
* LBIT <= 32 AND LBIT > 0
* BY A.E. SHEVEL 3/4-79
*
         SAVE  (14,12),,*
         BALR  9,0
         USING *,9
         LR    2,13
         LA    13,DSA##1     R13=DSA ADDRESS
         ST    13,8(2)
         ST    2,4(13)
         B     Y##1
DSA##1   DC    25F'0'        SAVE AREA
         CNOP  0,4
Y##1     EQU   *
***********************************
*
         LM    2,5,0(1)      LOAD PARAMETER ADDRESSES
         XR    0,0           R0=0
         L     6,0(3)             R6=NBEG
         LTR   6,6                NBEG=<0?
         BNH   ERRZ               YES
         SL    6,=F'1'       R6=R6-1
*
CONT     EQU   *
         L     7,0(4)        R7=LBIT
         LTR   7,7                LBIT=<0?
         BNH   ERRZ               YES
         C     7,=F'32'      R7 > 32?
         BNH   ARON          NO
         B     ERRZ               GO TO ERROR MESSAGE
ARON     EQU   *
         CLR   7,0           R7=0?
         BNZ   SHON          NO
         LA    7,1           R7=1
SHON     EQU   *
         LR    1,5           R1=R5
*
****************************************
*
* AT THIS POINT ALLOCATION OF DATA IS THE FOLLOWING:
*              R1 HAS ADDRESS OF RESULT
*              R2 HAS ADDRESS OF SOURCE BIT STRING
*              R4 HAS THE RESULT
*              R6 HAS (NBIT-1)
*              R7 HAS LBIT
*
****************************************
*
         LR    3,6           R3=R6
         SRL   3,3           R3=R3/8
* AT R3 WE OBTAINED A NUMBER OF BYTES
         N     6,MASK        R6=A NUMBER OF BIT IN BYTE
         ALR   2,3
         MVC   WRK,0(2)      WRK=SOURCE FIELD
         L     4,WRK         R4=WRK
         LR    5,6
         SLL   5,2           R5=R5*4
         N     4,M1(5)
*
*
         L     0,=F'32'      R0=32
         SLR   0,7           R0=R0-R7
         SLR   0,6           R0=R0-R6
         LTR   0,0
         BH    RSHIFT
         BZ    RET           GO OUT
*
*
*              LEFT SHIFT
*
LSHIFT   LPR   0,0           R0=ABS(R0)
         ST    0,INCR        STORE SHIFT PARAM.
         LR    8,0           R8=R0
         SLL   4,0(8)        SHIFT <--- BY R8
*
*              INCLUDE THE REST BITS
*
         LR    8,7           R8=R7
         SRL   8,3           R8=R8/8 NUMBER OF BYTES
         ALR   2,8           R2=R2+R8
         XR    0,0           R0=0
         IC    0,0(2)        R0=LAST BYTE
         N     7,MASK        NUMBER OF BITS
*
*              COMPUTE SHIFT PARAMETER
*
         SL    7,=F'8'       R7=R7-8
         LPR   7,7           R7=ABS(R7)
         SL    7,INCR        R7=R7-PREV. SHIFT PARAM.
         SRL   0,0(7)        SHIFT ---> BY R7
         OR    4,0
         B     RET           GO OUT
*
*
*
RSHIFT   LR    8,0           R8=R0
         SRL   4,0(8)        SHIFT --> BY R8
         B     RET           GO OUT
*
***********************************
*
*              DIAGNOSTICS
*
ERRZ     LM    2,5,0(1)
         ST    3,IB
         ST    4,JB
         LA    1,PERR             R1=ADDRESS OF PARAMETER LIST
         CALL  SHRERR
         B     RET
PERR     DS    0F
IB       DS    A
JB       DS    A
***********************************
*
RET      ST    4,0(1)        STORE RESULT
         L     13,DSA##1+4
         RETURN (14,12)
*
*              END OF PROGRAM TEXT
*
*****************************************************
*
*              CONSTANTS AND VARIABLES AREA
*
MASK     DC    F'7'          CONSTANT FOR LOG 'AND'
WRK      DC    F'0'          WORK VARIABLE
INCR     DS    F
M1       DS    0F
         DC    XL1'FF'       11111111
         DC    3X'FF'
         DC    FL1'127'      1111111
         DC    3X'FF'
         DC    FL1'63'       111111
         DC    3X'FF'
         DC    FL1'31'       11111
         DC    3X'FF'
         DC    FL1'15'       1111
         DC    3X'FF'
         DC    FL1'7'        111
         DC    3X'FF'
         DC    FL1'3'        11
         DC    3X'FF'
         DC    FL1'1'        1
         DC    3X'FF'
         DC    4X'00'        0
         END
#endif
#endif
