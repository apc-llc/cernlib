*
* $Id: bunch.s,v 1.1.1.1 1996/02/15 17:51:19 mclareni Exp $
*
* $Log: bunch.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:19  mclareni
* Kernlib
*
*
          IDENT BUNCH
*
* CERN PROGLIB# M436    BUNCH           .VERSION KERNCDC  1.16  820129
*
*      SUBROUTINE BUNCH (SOURCE,TARGET,NBYTES,NBITS)
*
*         SOURCE- GIVES THE ADDRESS OF A SOURCE ARRAY
*         TARGET- GIVES THE ADDRESS OF A TARGET ARRAY
*         NBYTES- NUMBER OF BYTES IN THE SOURCE RECORD
*         NBITS-  NUMBER OF BITS PER BYTE
*
*   J.D.BLAKE/CERN   1 OCTOBER 1974
*   MODIFIED JAN 1982 TO IGNORE CALLS WITH ERRONEOUS PARAMETER VALUES
*
          ENTRY BUNCH
 BUNCHTR  VFD   42/0LBUNCH,18/BUNCH
 BUNCHA0  DATA  0
 BUNCH    JP    400000B+*
          SX6   A0
          SA0   A1                A0=PARAMETER LIST ADDRESS
          SA6   BUNCHA0           SAVE OLD A0
          SB1   1                 CONSTANT B1=1
          SB2   60                CONSTANT B2=60
          SA1   A1                READ SOURCE ADDRESS
          SA2   A1+B1             READ TARGET ADDRESS
          SA3   A2+B1             READ ADDRESS OF NUMBER OF BYTES
          SA4   A3+B1             READ ADDRESS OF NUMBER OF BITS
          SA5   X1-1              READ WORD BEFORE FIRST SOURCE WORD
          SB6   X2                B6=TARGET WORD ADDRESS
          SA3   X3                READ NUMBER OF BYTES
          SA4   X4                READ NUMBER OF BITS PER BYTE
          SB3   X3                B3=BYTE COUNTER
          SB4   X4                B4=NUMBER OF BITS PER BYTE
          LE    B3,B0,BUNCH       RETURN IF NUMBER OF BYTES.LE.0
          LE    B4,B0,BUNCH       RETURN IF NUMBER OF BITS.LE.0
          GE    B4,B2,BUNCH       RETURN IF NUMBER OF BITS.GE.60
          SB5   B2                B5=BIT POSITION IN TARGET WORD
          MX7   1
          SB7   B4-59
          LX0   B7,X7             X0=BYTE MASK
          SX6   B0                CLEAR FIRST TARGET WORD
*
**        MAIN LOOP - ONCE PER BYTE
*
*         REGISTER CONVENTIONS
*           A5,X5      SOURCE WORD
*           A6,B6,X6   TARGET WORD
*           X0         BYTE MASK (COMPLEMENT)
*           B1         1
*           B2         60
*           B3         BYTE COUNTER (=NBYTES INITIALLY, =0 FINALLY)
*           B4         BYTE LENGTH
*           B5         BIT POSITION IN TARGET WORD (=60 INITIALLY)
*
 BUNCH1   SA5   A5+B1             READ NEXT SOURCE WORD
          SB5   B5-B4             DECREMENT BIT POSITION
          LE    B3,B0,BUNCH3      JUMP IF LAST BYTE HAS BEEN MOVED
 BUNCH2   BX2   -X0*X5            EXTRACT SOURCE BYTE
          LX3   B5,X2             POSITION SOURCE BYTE (END-OFF)
          SB3   B3-B1             DECREMENT BYTE COUNTER
          BX6   X6+X3             ADD SOURCE BYTE TO TARGET WORD
          GT    B5,B0,BUNCH1      LOOP UNLESS TARGET WORD IS FULL
          SA6   B6                STORE TARGET WORD
          SB5   B5+B2             RESET BIT POSITION (ADD 60)
          SB6   B6+B1             INCREMENT TARGET ADDRESS
          LX4   B5,X2             POSITION SOURCE BYTE (CIRCULARLY)
          SA5   A5+B1             READ NEXT SOURCE WORD
          SB5   B5-B4             DECREMENT BIT POSITION
          BX6   X4-X3             STORE BOTTOM OF SOURCE BYTE IN TARGET
          GT    B3,B0,BUNCH2      LOOP UNLESS LAST BYTE HAS BEEN MOVED
*
**        END OF LOOP - STORE INCOMPLETE TARGET WORD, IF ANY,
*         AND RETURN
*
 BUNCH3   SA4   BUNCHA0
          SB7   B5+B4             BIT POSITION OF LAST BYTE
          SA0   X4                RESTORE OLD A0
          EQ    B7,B2,BUNCH       RETURN IF TARGET WORD IS EMPTY
          SA6   B6                STORE INCOMPLETE TARGET WORD
          EQ    BUNCH             RETURN
          END
#ifdef CERNLIB_TCGEN_BUNCH
#undef CERNLIB_TCGEN_BUNCH
#endif
