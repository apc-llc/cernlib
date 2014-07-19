*
* $Id: i16itc.s,v 1.1.1.1 1996/03/08 15:21:53 mclareni Exp $
*
* $Log: i16itc.s,v $
* Revision 1.1.1.1  1996/03/08 15:21:53  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CDC)
          IDENT       I16ITC
          ENTRY       I16ITC
* SUBROUTINE I16ITC(ARRAY,NWORDS)
*
*   THE ROUTINE CONVERTS 16 BIT INTEGERS (INCLUDING SIGN) FROM
*   IBM TO CDC FORMAT ( FFFF=-1 ON IBM, 77...77 = -0 ON CDC )
*   AUTHOR H. GROTE CERN DD
*
*   THE SUBROUTINE EXPECTS TWO PARAMETERS
*   ARRAY- GIVES THE ADDRESS OF THE INPUT/ OUTPUT ARRAY
*   NW - THE NUMBER OF WORDS TO BE CONVERTED
*   A WORD COUNT LE ZERO ACTS AS DO NOTHING.
*
 TRACE    VFD         42/0LI16ITC,18/I16ITC
 STOA0    BSS    1
 I16ITC   JP          400000B+*
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
 EXS0     GE          B2,B3,EXIT       EXIT JUMP
          SA1         B2               GET A WORD
          LX3         B4,X1            LEFT SHIFT 44 POS.
          PL          X3,CNT           JUMP IF POSITIVE
          AX4         B4,X3            RIGHT SHIFT AND SIGN EXT. 44 POS.
          SX6         X4-1             BECAUSE OF DIFF. NEG. REP.
          SA6         B2               STORE
 CNT      SB2         B2+B1            INCREASE COUNT
          EQ          EXS0             LOOP
 EXIT     SA1    STOA0
          SA0    X1                RESTORE A0
          EQ     I16ITC
          END
#endif
