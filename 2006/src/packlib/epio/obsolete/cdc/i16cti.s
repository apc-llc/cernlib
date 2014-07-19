*
* $Id: i16cti.s,v 1.1.1.1 1996/03/08 15:21:53 mclareni Exp $
*
* $Log: i16cti.s,v $
* Revision 1.1.1.1  1996/03/08 15:21:53  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CDC)
          IDENT       I16CTI
          ENTRY       I16CTI
* SUBROUTINE I16CTI(ARRAY,NWORDS)
*
*   THE ROUTINE CONVERTS 16 BIT INTEGERS (INCLUDING SIGN) FROM
*   CDC TO IBM FORMAT ( FFFF=-1 ON IBM, 77...77 = -0 ON CDC )
*   AUTHOR H. GROTE CERN DD
*
*   THE SUBROUTINE EXPECTS TWO PARAMETERS
*   ARRAY- GIVES THE ADDRESS OF THE INPUT/ OUTPUT ARRAY
*   NW - THE NUMBER OF WORDS TO BE CONVERTED
*   A WORD COUNT LE ZERO ACTS AS DO NOTHING.
*
 TRACE    VFD         42/0LI16CTI,18/I16CTI
 STOA0    BSS    1
 I16CTI   JP          400000B+*
          SX6    A0
          SA6    STOA0              STORE A0
          SA3         A1               ADDRESS OF FIRST ARGUMENT
          SA4         A1+1             ADDRESS OF SECOND ARGUMENT
          SB1         1
          SA1         X4               SECOND ARGUMENT = NW INTO X1
          IX5         X3+X1
          SB2         X3               ADDRESS OF FIRST WORD
          SB3         X5               ADDRESS OF LAST WORD + 1
          SB4         44               SHIFT CONSTANT
          MX5         44               MASK
 EXS0     GE          B2,B3,EXIT       EXIT JUMP
          SA1         B2               GET A WORD
          PL          X1,CNT           JUMP IF POSITIVE ( SIGN BIT TEST)
          NZ          X1,EXS1          JUMP IF NOT ZERO (LEAVES -0 )
          SX6         B0               SET -0 TO 0
          EQ          STORE
 EXS1     BX4         -X5*X1           MASK
          SX6         X4+B1            DIFFERENT NEG. REP.
 STORE    SA6         B2               STORE
 CNT      SB2         B2+B1            INCREASE COUNT
          EQ          EXS0             LOOP
 EXIT     SA1    STOA0
          SA0    X1                RESTORE A0
          EQ     I16CTI
          END
#endif
