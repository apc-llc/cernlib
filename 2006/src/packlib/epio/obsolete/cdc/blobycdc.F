*
* $Id: blobycdc.F,v 1.1.1.1 1996/03/08 15:21:53 mclareni Exp $
*
* $Log: blobycdc.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:53  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CDC)
          IDENT       BLOBYT
          ENTRY       BLOBYT
* SUBROUTINE BLOBYT(SOURCE,TARGET,MBYTES,NBITS,NSTART)
*
*   THE ROUTINE CONVERTS A SOURCE ARRAY CONTAINING A RECORD
*   OF NBYTES WITH NBITS IN A 60 BITS WORD TARGET ARRAY
*
*   THIS IS AN ADAPTION OF ROUTINE -BLOW- IN CERN LIBRARY
*   AUTHOR H. GROTE CERN DD
*
*   THE SUBROUTINE EXPECTS FIVE PARAMETER
*   SOURCE- GIVES THE ADDRESS OF A SOURCE ARRAY
*   TARGET- GIVES THE ADDRESS OF A TARGET ARRAY
*   NBYTES- NUMBER OF BYTES IN THE SOURCE RECORD
*   NBITS- NUMBER OF BITS PER BYTE
*   NSTART- POSITION OF FIRST BIT OF FIRST BYTE IN SOURCE
*           ( COUNT STARTS WITH 1 AT LEFT )
*
 TRACE    VFD         42/0LBLOBYT,18/BLOBYT
 BLOWA0   BSSZ        1
 BLOBYT   JP          400000B+*
          SX6         A0
          SA6         BLOWA0           SAVE OLD A0
          SB1         1                CONSTANT B1=1
          SB6         60               CONSTANT B6=60
          SA0         A1               A0=PARAMETER LIST ADDRESS
          SA5    A0+4              NSTART=STARTING BIT POSITION
          SA5    X5
          SX3    B6
          PX0    B0,X3
          NX6    B0,X0             60. IN FLOATING
          SX4    1
          IX0    X5-X4
          PX5    B0,X0             NSTART -1 IN FLOATING
          FX4    X5/X6
          UX5    B2,X4
          LX6    B2,X5             (NSTART - 1 )/60 IN INTEGER
          SB7    X6                ADDRESS OF FIRST WORD REL. TO SOURCE
          DX5    X3*X6
          IX4    X0-X5             STARTING BIT -1 IN WORD
          SB3    X4
          SA1         A1               READ SOURCE ADDRESS
          SA2         A1+B1            READ TARGET ADDRESS
          SA3         A2+B1            READ ADDRESS OF NUMBER OF BYTES
          SA4         A3+B1            READ ADDRESS OF NUMBER OF BITS
          SA5         X1+B7            READ FIRST SOURCE WORD
          LX5    B3,X5             LEFT SHIFT BYTE TO LEFT BOUNDARY
          SB4         X2               B4=TARGET WORD ADDRESS
          SA3         X3               READ NUMBER OF BYTES
          SA4         X4               READ NUMBER OF BITS PER BYTE
          SB5         X3+B4            B5=TARGET WORD LIMIT(LWA+1)
          SB2         X4               B2=NUMBER OF BITS PER BYTE
          GE          B4,B5,BLOW2      RETURN IF NUMBER OF BYTES .LE.0
          GE          B0,B2,BLOW2      ERROR IF NUMBER OF BITS.LE.0
          GE          B2,B6,BLOW2      ERROR IF NUMBER OF BITS.GE.60
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
          EQ          BLOBYT           EXIT
          END
#endif
