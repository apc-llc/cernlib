*
* $Id: btswapcd.F,v 1.1.1.1 1996/03/08 15:21:52 mclareni Exp $
*
* $Log: btswapcd.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:52  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CDC)
          IDENT  BTSWAP
          ENTRY  BTSWAP
*-----------------------------------------------------------
*
* SUBROUTINE BTSWAP(IARRAY,NBYTES)
*
*  MOVES 16 BIT WORDS IN PACKED FORM.
*
* ARGUMENTS
* IARRAY     SOURCE ARRAY
* NBYTES     NO. OF BYTES TO SWAP, STARTING AT THE LEFTMOST
*            POSITION IN IARRAY. IF LE 1, NO ACTION. IF ODD,
*            NBYTES-1 IS USED INSTEAD.
*
* AUTHOR  H. GROTE CERN/DD  5.11.84
*
*-----------------------------------------------------------
 TRACE    VFD    42/0LBTSWAP,18/BTSWAP
*--- MASK FOR 6 BYTES = 8*0, 8*1, 8*0, 8*1, 8*0, 8*1, 12*0
 MASK     DATA   00177400776003770000B
 STOA0    BSS    1
 BTSWAP   JP     400000B+*
          SX6    A0
          SA6    STOA0              STORE A0
          SA3    A1
          SA4    A1+1
          SA1    X4                NBYTES INTO X1
          AX1    1                 DIVIDE BY 2
          SB1    X1                NO. OF SWAPS TO PERFORM
          SA0    X3                ADDRESS OF FIRST WORD
          SB6    60
          SB2    B6
          SA1    MASK
          BX0    X1                MASK INTO X0
*
* USE OF REGISTERS
*
*   B1   NO. OF SWAPS = LOOP INDEX
*   B2   NO. OF BITS LEFT IN CURRENT WORD
*   B3   NO. OF WORDS TO TAKE FROM NEXT WORD
*   B4   NO. OF BITS TO TREAT IN ONE LOOP CYCLE (NORMALLY 48)
*   B5   NO. OF BITS TO TREAT IN CURRENT WORD
*   B6   =60
*   B7   NO. OF BITS TAKEN FROM FIRST WORD
*
*   X0   MASK FOR BYTE SWAPPING
*   X7   MASK FOR RESTORING INTO SECOND WORD
*
*   A0   ADDRESS OF CURRENT WORD
*   A2   ADDRESS OF FIRST WORD
*
 LOOP     LE     B1,B0,EXIT        LOOP TERMINATION
          SA2    A0                LOAD WORD IN X2
          SB3    3                 MAX. NO. OF SWAPS/CYCLE
          GE     B1,B3,CONT1
          SB3    B1                LESS THAN 3 LEFT
 CONT1    SB1    B1-B3             REDUCE LOOP COUNTER
          SX6    B3
          LX6    4                 MULTIPLY BY 16 TO GET NO. OF BITS
          SB4    X6                NO. OF BITS THIS CYCLE
          SB5    B6-B2
          LX3    B5,X2             LEFT ADJUST BYTES TO SWAP
          SB5    B4
          GE     B2,B4,CONT2
          SB5    B2                B5 = MIN(B2,B4)
 CONT2    SX4    1
          SB7    B5-1
          LX4    -1
          AX5    B7,X4             MASK OF B5 BITS LEFT ADJ.
          BX3    X3*X5
          SB7    B2                KEEP B2 IN B7
          SB2    B2-B4
          SB3    B0
          GT     B2,B0,SWAP
          EQ     B2,B0,CONT3
          SB3    -B2               NO. OF BITS TO TAKE FROM NEXT WORD
 CONT3    SB2    B2+B6             INCREASE NO. OF BITS BY NEW WORD
          SA0    A0+1              INCREASE ADDRESS
          EQ     B3,B0,SWAP        JUMP IF NOTHING TAKEN FROM NEW WORD
          SA1    A0                NEW WORD INTO X1
          SX4    1
          SB4    B3-1
          LX4    -1
          AX7    B4,X4             MASK OF B3 LEFT-ADJ. BITS
          BX2    X1*X7
          SB4    B6-B7
          LX2    B4,X2             RIGHT SHIFT BITS FROM NEW WORD BY B7
          BX3    X2+X3
*--- NOW 6 (OR LESS) 8-BIT BYTES LEFT ADJUSTED IN X3
 SWAP     BX4    X0*X3             MASK EVEN BYTES
          LX4    8                 MAKE ODD
          LX3    -8                MAKE ODD BYTES EVEN
          BX3    X0*X3             MASK
          BX5    X3+X4             BYTES ARE SWAPPED IN X4
*--- RESTORE IN FIRST (AND POSSIBLY SECOND) WORD
          SA1    A2                GET FIRST WORD IN X1
          SB4    B6-B7             NO. OF UNCHANGED BITS, WORD 1
          LX2    B4,X1             RIGHT-ADJ. UNCHANGED BITS
          SX3    1
          SB4    B5-1
          LX3    -1
          AX3    B4                MASK OF 1-BITS FOR BITS TO CHANGE
          BX4    -X3*X2            UNCHANGED PART
          BX1    X3*X5             NEW (SWAPPED) BITS
          BX6    X1+X4             JOIN
          LX6    B7,X6             SHIFT INTO ORIGINAL POSITION
          SA6    A2                RESTORE FIRST WORD
          EQ     B3,B0,LOOP        JUMP IF SECOND WORD UNUSED
          SA1    A0                SECOND WORD INTO X1
          BX3    -X7*X1            UNCHANGED PART
          LX4    B7,X5             LEFT-ADJ. CHANGED PART IN RESULT
          BX5    X7*X4
          BX6    X3+X5             JOIN
          SA6    A0                RESTORE
          EQ     LOOP
 EXIT     SA1    STOA0
          SA0    X1                RESTORE A0
          EQ     BTSWAP
          END
#endif
