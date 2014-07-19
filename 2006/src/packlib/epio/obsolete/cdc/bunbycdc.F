*
* $Id: bunbycdc.F,v 1.1.1.1 1996/03/08 15:21:53 mclareni Exp $
*
* $Log: bunbycdc.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:53  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CDC)
          IDENT  BUNBYT
          SPACE  2
***       SUBROUTINE BUNBYT(SOURCE,TARGET,NBYTES,NBITS,NSTART)
*
*         BUNBYT CONVERTS AN ARRAY OF BYTES, STORED ONE PER 60-BIT
*         WORD, RIGHT JUSTIFIED, INTO A CONTIGUOUS BIT STRING, STARTING
*         AT BIT POSITION NSTART IN TARGET ARRAY (COUNT STARTS WITH 1
*         AT LEFT )
*
*         ON ENTRY
*           SOURCE IS THE SOURCE ARRAY
*           NBYTES IS THE NUMBER OF BYTES IN THE SOURCE ARRAY
*           NBITS IS THE NUMBER OF BITS PER BYTE
*           NSTART IS THE STARTING BIT POSITION FOR THE FIRST BYTE
*           TO BE STORED IN TARGET.
*
*         ON EXIT
*           TARGET IS THE TARGET ARRAY. BIT POSITIONS IN TARGET NOT
*           CONCERNED BY THE MOVE REMAIN UNTOUCHED.
*
*         ERROR CONDITIONS
*           NBITS.LE.0 OR NBITS.GE.60
*
*         THIS VERSION FOR FORTRAN EXTENDED (FORTRAN COMMON LIBRARY)
*         UNDER 6000 SCOPE 3.4 OR 7000 SCOPE 2.0.
*
*         THIS ROUTINE IS A MODIFIED VERSION OF ROUTINE -BUNCH-
*         IN THE CERN LIBRARY.
*
*         H. GROTE/CERN   3 JUNE 1980
          SPACE  3
          ENTRY  BUNBYT
          SPACE  1
          LIST   G
          SPACE  2
 BUNCHTR  VFD    42/0LBUNBYT,18/BUNBYT
 BUNCHA0  DATA   0
 BUNBYT   JP     400000B+*
          SX6    A0
          SA0    A1                A0=PARAMETER LIST ADDRESS
          SA6    BUNCHA0           SAVE OLD A0
          SB1    1                 CONSTANT B1=1
          SB2    60                CONSTANT B2=60
          SA5    A0+4              NSTART=STARTING BIT POSITION
          SA5    X5
          SX3    B2
          PX0    B0,X3
          NX6    B0,X0             60. IN FLOATING
          SX4    1
          IX0    X5-X4
          PX5    B0,X0             NSTART -1 IN FLOATING
          FX4    X5/X6
          UX5    B6,X4
          LX6    B6,X5             (NSTART - 1 )/60 IN INTEGER
          SB7    X6                ADDRESS OF FIRST WORD REL. TO SOURCE
          DX5    X3*X6
          IX4    X0-X5             STARTING BIT -1 IN WORD
          SB5    X4
          SA1    A1                READ SOURCE ADDRESS
          SA2    A1+B1             READ TARGET ADDRESS
          SA3    A2+B1             READ ADDRESS OF NUMBER OF BYTES
          SA4    A3+B1             READ ADDRESS OF NUMBER OF BITS
          SA5    X1-1              READ WORD BEFORE FIRST SOURCE WORD
          SB6    X2+B7             B6=TARGET WORD ADDRESS
          SA3    X3                READ NUMBER OF BYTES
          SA4    X4                READ NUMBER OF BITS PER BYTE
          SB3    X3                B3=BYTE COUNTER
          SB4    X4                B4=NUMBER OF BITS PER BYTE
          LE     B3,B0,BUNCH4      RETURN IF NUMBER OF BYTES.LE.0
          LE     B4,B0,BUNCH4      ERROR IF NUMBER OF BITS.LE.0
          GE     B4,B2,BUNCH4      ERROR IF NUMBER OF BITS.GE.60
          EQ     B5,B0,BUNCH7
          SA2    B6                FIRST TARGET WORD
          MX7    1
          SB7    B5-1
          AX0    B7,X7             MASK
          BX6    X0*X2             MASK FIRST TARGET WORD
          EQ     BUNCH8
 BUNCH7   SX6    B0                CLEAR TARGET WORD IF AT LEFT BOUNDARY
 BUNCH8   SB5    B2-B5             B5=BIT POSITION IN TARGET WORD
          MX7    1
          SB7    B4-59
          LX0    B7,X7             X0=BYTE MASK
          SPACE  2
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
*           B5         BIT POSITION IN TARGET WORD
          SPACE  2
 BUNCH1   SA5    A5+B1             READ NEXT SOURCE WORD
          SB5    B5-B4             DECREMENT BIT POSITION
          LE     B3,B0,BUNCH3      JUMP IF LAST BYTE HAS BEEN MOVED
 BUNCH2   BX2    -X0*X5            EXTRACT SOURCE BYTE
          LX3    B5,X2             POSITION SOURCE BYTE (END-OFF)
          SB3    B3-B1             DECREMENT BYTE COUNTER
          BX6    X6+X3             ADD SOURCE BYTE TO TARGET WORD
          GT     B5,B0,BUNCH1      LOOP UNLESS TARGET WORD IS FULL
          SA6    B6                STORE TARGET WORD
          SB5    B5+B2             RESET BIT POSITION (ADD 60)
          SB6    B6+B1             INCREMENT TARGET ADDRESS
          LX4    B5,X2             POSITION SOURCE BYTE (CIRCULARLY)
          SA5    A5+B1             READ NEXT SOURCE WORD
          SB5    B5-B4             DECREMENT BIT POSITION
          BX6    X4-X3             STORE BOTTOM OF SOURCE BYTE IN TARGET
          GT     B3,B0,BUNCH2      LOOP UNLESS LAST BYTE HAS BEEN MOVED
          SPACE  2
**        END OF LOOP - STORE INCOMPLETE TARGET WORD, IF ANY,
*         AND RETURN
          SPACE  2
 BUNCH3   SB7    B5+B4             BIT POSITION OF LAST BYTE
          EQ     B7,B2,BUNCH4       RETURN IF TARGET WORD IS EMPTY
          SB3    B2-B7
          SB7    B3-B1
          MX7    1
          AX7    B7,X7              MASK FOR LAST TARGET WORD
          SA2    B6                 GET LAST TARGET WORD
          BX3    -X7*X2
          BX6    X6+X3              JOIN TARGET WORD WITH REST OF BYTE
          SA6    B6                 STORE LAST TARGET WORD
 BUNCH4   SA4    BUNCHA0
          SA0    X4                RESTORE OLD A0
          EQ     BUNBYT             RETURN
          SPACE  2
          END
#endif
