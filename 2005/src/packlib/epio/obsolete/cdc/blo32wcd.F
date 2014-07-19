*
* $Id: blo32wcd.F,v 1.1.1.1 1996/03/08 15:21:52 mclareni Exp $
*
* $Log: blo32wcd.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:52  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CDC)
          IDENT       BLO32W
          ENTRY       BLO32W
* SUBROUTINE BLO32W(SOURCE,N1,TARGET,N2,N3)
*
*   THIS IS AN ADAPTION OF ROUTINE -BLOW- IN CERN LIBRARY
*   AUTHOR H. GROTE CERN DD
*
*   BLOWS (UNPACKS) 32 BIT WORDS INTO MACHINE WORDS.
*
*
*    +++ IMPORTANT +++ COUNTS PARTLY IN 16 BIT WORDS
*
*    INPUT
*    SOURCE    ARRAY CONTAINING 32 BIT WORDS IN PACKED FORM
*    N1        FIRST 16 BIT WORD IN SOURCE TO BLOW
*    TARGET    TARGET ARRAY FOR UNPACKED 32 BIT WORDS
*    N2        TARGET(N2) WILL CONTAIN FIRST UNPACKED WORD
*    N3        NO. OF 32 BIT WORDS TO UNPACK
*
 TRACE    VFD         42/0LBLO32W,18/BLO32W
 BLOWA0   BSSZ        1
 F60      DATA        17257400000000000000B
 BLO32W   JP          400000B+*
          SX6         A0
          SA6         BLOWA0           SAVE OLD A0
          SB1         1                CONSTANT B1=1
          SA0    A1
          SB2    2
          SA5    A0+1              N1 = 1ST 16 BIT WORD ADDRESS
          SB3    4
          SX4    1
          SA2    X5                N1 TO X2
          SA1    F60               60. TO X1
          IX0    X2-X4             N1-1 TO X0
          SB6    60
          LX2    B3,X0             X2 = 16*(N1-1)
          PX5    B0,X2
          FX4    X5/X1
          SX3    B6
          UX5    B3,X4
          LX6    B3,X5             16*(N1-1)/60 IN INTEGER
          SB7    X6                ADDRESS OF FIRST WORD IN SOURCE
          DX5    X3*X6
          IX4    X2-X5             STARTING BIT-1 IN WORD
          SB3    X4
          SA1    A0                X1 = SOURCE ADDRESS
          SA2    A1+B2             X2 = TARGET ADDRESS
          SA3    A2+B1             X3 = ADDRESS OF FIRST REL. TARGET WORD
          SA4    A3+B1             X4 = ADDR. OF NO. OF BYTES
          SA5    X1+B7             READ FIRST SOURCE WORD
          SA1    X3                FIRST TARGET WORD COUNT TO X1
          SB5    X1-1              REL. ADDRESS OF FIRST TARGET WORD
          LX5    B3,X5             LEFT SHIFT BYTE TO BOUNDARY
          SB4    X2+B5             FIRST TARGET WORD ADDRESS
          SA3    X4                X3 = NO. OF BYTES TO TRANSFER
          ZR     X3,BLOW2          EXIT IF NUMBER OF BYTES=0
          NG     X3,BLOW2          EXIT IF NUMBER OF BYTES LT 0
          SB5    X3+B4             B5 = TARGET WORD LIMIT +1
          SB2    32                NO. OF BITS/BYTE
          SB7         B6-B2            B7=REMAINING BIT COUNT IN WORD
          MX4         1
          SB7         B7-B1
          AX7         B7,X4            X7=BYTE MASK
          SB3    B6-B3             SET REMAINING BIT COUNT
          SB5         B5-B1            B5=LAST TARGET WORD ADDRESS
          EQ     BLOW6
*
*    MAIN LOOP - ONCE PER BYTE
*
 BLOW1    SA6         B4               STORE BYTE
          EQ          B4,B5,BLOW2      JUMP IF LAST BYTE
          SB3         B3-B2            DECREMENT REMAINING BIT COUNT
          SB4         B4+B1            INCREMENT TARGET ADDRESS
 BLOW6    LX5         B2,X5            SHIFT SOURCE WORD
          BX6         -X7*X5           MASK OUT BYTE
          LT          B2,B3,BLOW1      LOOP UNLESS AT SOURCE WORD BOUND
          SA5         A5+B1            READ NEXT SOURCE WORD
          AX4         B3,X7            CONSTRUCT PARTIAL BYTE MASK
          BX2         X4*X6            MASK OUT LEFT PARTIAL BYTE
          SB7         B2-B3            NUMBER OF BITS FROM NEW WORD
          LX5         B7,X5            SHIFT NEW SOURCE WORD
          BX3         -X4*X5           MASK OUT RIGTH PARTIAL BYTE
          BX6         X2+X3            MERGE PARTIAL BYTES
          SB3         B6+B3            RESET REMAINING BIT COUNT
          EQ          BLOW1
*
*    NORMAL EXIT
*
 BLOW2    SA4         BLOWA0
          SA0         X4               RESTORE OLD AO
          EQ          BLO32W           EXIT
          END
#endif
