*
* $Id: blow.s,v 1.1.1.1 1996/02/15 17:51:19 mclareni Exp $
*
* $Log: blow.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:19  mclareni
* Kernlib
*
*
          IDENT BLOW
*
* CERN PROGLIB# M426    BLOW            .VERSION KERNCDC  1.16  820129
*
*     SUBROUTINE BLOW (SOURCE,TARGET,NBYTES,NBITS)
*
*         SOURCE- GIVES THE ADDRESS OF A SOURCE ARRAY
*         TARGET- GIVES THE ADDRESS OF A TARGET ARRAY
*         NBYTES- NUMBER OF BYTES IN THE SOURCE RECORD
*         NBITS-  NUMBER OF BITS PER BYTE
*
*   MODIFIED JAN 1982 TO IGNORE CALLS WITH ERRONEOUS PARAMETER VALUES
*
          ENTRY BLOW
 TRACE    VFD   42/0LBLOW,18/BLOW
 BLOWA0   BSSZ  1
 BLOW     JP    400000B+*
          SX6   A0
          SA6   BLOWA0           SAVE OLD A0
          SB1   1                CONSTANT B1=1
          SB6   60               CONSTANT B6=60
          SA0   A1               A0=PARAMETER LIST ADDRESS
          SA1   A1               READ SOURCE ADDRESS
          SA2   A1+B1            READ TARGET ADDRESS
          SA3   A2+B1            READ ADDRESS OF NUMBER OF BYTES
          SA4   A3+B1            READ ADDRESS OF NUMBER OF BITS
          SA5   X1               READ FIRST SOURCE WORD
          SB4   X2               B4=TARGET WORD ADDRESS
          SA3   X3               READ NUMBER OF BYTES
          SA4   X4               READ NUMBER OF BITS PER BYTE
          SB5   X3+B4            B5=TARGET WORD LIMIT(LWA+1)
          SB2   X4               B2=NUMBER OF BITS PER BYTE
          GE    B4,B5,BLOW       RETURN IF NUMBER OF BYTES .LE.0
          GE    B0,B2,BLOW       RETURN IF NUMBER OF BITS.LE.0
          GE    B2,B6,BLOW       RETURN IF NUMBER OF BITS.GE.60
          SB3   B6-B2            B3=REMAINING BIT COUNT IN WORD
          MX4   1
          SB7   B3-B1
          LX5   B2,X5            SHIFT FIRST SOURCE WORD
          AX7   B7,X4            X7=BYTE MASK
          BX6   -X7*X5           MASK OUT FIRST BYTE
          SB3   B3+B2            RESET REMAINING BIT COUNT
          SB5   B5-B1            B5=LAST TARGET WORD ADDRESS
*
*    MAIN LOOP - ONCE PER BYTE
*
 BLOW1    SA6   B4               STORE BYTE
          EQ    B4,B5,BLOW2      JUMP IF LAST BYTE
          SB3   B3-B2            DECREMENT REMAINING BIT COUNT
          LX5   B2,X5            SHIFT SOURCE WORD
          SB4   B4+B1            INCREMENT TARGET ADDRESS
          BX6   -X7*X5           MASK OUT BYTE
          LT    B2,B3,BLOW1      LOOP UNLESS AT SOURCE WORD BOUND
          SA5   A5+B1            READ NEXT SOURCE WORD
          AX4   B3,X7            CONSTRUCT PARTIAL BYTE MASK
          BX2   X4*X6            MASK OUT LEFT PARTIAL BYTE
          SB7   B2-B3            NUMBER OF BITS FROM NEW WORD
          LX5   B7,X5            SHIFT NEW SOURCE WORD
          BX3   -X4*X5           MASK OUT RIGTH PARTIAL BYTE
          BX6   X2+X3            MERGE PARTIAL BYTES
          SB3   B6+B3            RESET REMAINING BIT COUNT
          EQ    BLOW1
*
*    NORMAL EXIT
*
 BLOW2    SA4   BLOWA0
          SA0   X4               RESTORE OLD AO
          EQ    BLOW             EXIT
          END
#ifdef CERNLIB_TCGEN_BLOW
#undef CERNLIB_TCGEN_BLOW
#endif
