*
* $Id: stoasccd.F,v 1.1.1.1 1996/03/08 15:21:52 mclareni Exp $
*
* $Log: stoasccd.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:52  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CDC)
          IDENT  STOASC
          SPACE  2
***       SUBROUTINE STOASC(SOURCE,N1,TARGET,N2,NCH)
*
*         STOASC CONVERTS A STRING OF CDC DISPLAY CHARACTERS
*         INTO A STRING OF 8-BIT ASCII CHARACTERS.
*
*         PARAMETERS
*           SOURCE           SOURCE ARRAY
*           N1               FIRST CHARACTER IN SOURCE TO CONVERT
*           TARGET           TARGET ARRAY
*           N2               FIRST 8-BIT BYTE IN TARGET TO STORE IN
*           NCH              NO. OF CHARACTERS TO CONVERT
*
*
*
*
*         H. GROTE/CERN   14 MARCH 1983
          SPACE  3
          ENTRY  STOASC
          SPACE  1
          LIST   G
          SPACE  2
 STOCHTR  VFD    42/0LSTOASC,18/STOASC
 STOCHA0  BSS    1
 STOCHA1  BSS    1
 FIRST    DATA   0
 ZERO     DATA   0
 ARG      BSS    2
          USE    /ASCIIC/
 ITASC    BSS    8
 ITCDC    BSS    16
          USE    CODE.
          EXT    CFRASC
 STOASC   JP     400000B+*
          SX6    A0
          SA0    A1                A0=PARAMETER LIST ADDRESS
          SA6    STOCHA0           SAVE OLD A0
*
*       CALL CFRASC FOR COMMON BLOCK INIT. FIRST TIME
*
          SA1    FIRST
          SB1    X1
          GT     B1,B0,START       SKIP IF NOT FIRST TIME
          SX6    1
          SA6    FIRST
          SA1    ZERO
          SX6    A1
          SA6    ARG
          SA6    ARG+1B
          SA1    ARG
          SX6    A0
          SA6    STOCHA1
          RJ     CFRASC
          SA1    STOCHA1
          SA0    X1
START     SB1    1                 CONSTANT B1=1
          SB2    60                CONSTANT B2=60
          SA5    A0+3              N2 = STARTING BYTE IN TARGET
          SA5    X5
          SX3    B2
          SB3    3
          PX0    B0,X3
          NX6    B0,X0             60. IN FLOATING
          BX1    X6                KEEP IN X1
          SX4    1
          IX0    X5-X4             N2-1
          LX0    B3,X0             8*(N2-1)
          PX5    B0,X0             8*(N2-1) IN FLOATING
          FX4    X5/X6
          UX5    B6,X4
          LX6    B6,X5             8*(N2-1)/60 IN INTEGER
          SB7    X6                ADDRESS OF FIRST WORD REL. TO TARGET
          DX5    X3*X6
          IX4    X0-X5             STARTING BIT -1 IN WORD
          SB5    X4                KEEP STARTING BIT IN TARGET WORD
          SB3    2
          SA5    A0+1              N1 = FIRST CH. IN SOURCE
          SA5    X5
          SX4    1
          IX0    X5-X4             N1-1
          LX4    B3,X0
          LX0    B1,X0
          IX0    X0+X4             6*(N1-1)
          PX5    B0,X0
          FX4    X5/X1
          UX5    B6,X4
          LX6    B6,X5
          SB2    X6                FIRST WORD IN SOURCE
          DX5    X3*X6
          IX7    X0-X5             STARTING BIT IN SOURCE
          SB6    X7                STORE IN B6
          SA1    A0                READ SOURCE ADDRESS
          SA2    A1+B3             READ TARGET ADDRESS
          SA3    A2+B3             READ ADDRESS OF NUMBER OF BYTES
          SA5    X1+B2             FIRST SOURCE WORD
          SB2    60
          SA0    X2+B7             A0=TARGET WORD ADDRESS
          SA3    X3                READ NUMBER OF BYTES
          SB3    X3                B3=BYTE COUNTER
          SB4    8                 B4=NUMBER OF BITS PER BYTE
          LE     B3,B0,STOCH4      RETURN IF NUMBER OF BYTES.LE.0
          EQ     B5,B0,STOCH7
          SA2    A0                FIRST TARGET WORD
          MX7    1
          SB7    B5-1
          AX0    B7,X7             MASK
          BX6    X0*X2             MASK FIRST TARGET WORD
          EQ     STOCH8
 STOCH7   SX6    B0                CLEAR TARGET WORD IF AT LEFT BOUNDARY
 STOCH8   SB5    B2-B5             B5=BIT POSITION IN TARGET WORD
          SX7    77B               CHARACTER MASK
          SX1    177B              ASCII CHARACTER MASK
          SB7    6                 CHARACTER LENGTH
          LX5    B6,X5             FIRST CHARACTER TO LEFT BOUNDARY IN X5
          SPACE  2
**        MAIN LOOP - ONCE PER BYTE
*
*         REGISTER CONVENTIONS
*           X1         ASCII CHARACTER MASK
*           X5         SOURCE WORD
*           X6         TARGET WORD
*           X7         DISPLAY CHARACTER MASK
*
*           A0         TARGET WORD ADDRESS
*           A5         SOURCE WORD ADDRESS
*
*           B1         1
*           B2         60
*           B3         BYTE COUNTER
*           B4         8
*           B5         BIT POSITION IN TARGET WORD
*           B6         BIT POSITION IN SOURCE WORD
*           B7         6
*
          SPACE  2
 STOCH1   SB5    B5-B4             DECREMENT BIT POSITION
          LE     B3,B0,STOCH3      JUMP IF LAST BYTE HAS BEEN MOVED
          LX5    B7,X5             NEXT CHARACTER RIGHT ADJUSTED
          SB6    B6+B7             COUNT BITS
          BX2    X7*X5             MASK CHARACTER INTO X2 RIGHT ADJ.
          GT     B2,B6,STOCH2      JUMP IF SOURCE WORD NOT COMPLETED
          SB6    B0                RESET SOURCE WORD BIT COUNTER
          SA5    A5+B1             LOAD NEXT SOURCE WORD
*
*        FOR CONVERSION, SEE ROUTINES CTOASC, CFRASC
*
 STOCH2   SB2    X2-1
          SX3    B2
          SB2    3
          AX3    B2,X3             (K-1)/8
          SB1    X3
          LX3    B2,X3
          IX3    X2-X3
          SX4    7
          DX3    X4*X3
          SB2    X3
          SA4    ITASC+B1
          LX4    B2,X4
          BX2    X4*X1
          SB1    1
          SB2    60
          LX3    B5,X2             POSITION SOURCE BYTE (END-OFF)
          SB3    B3-B1             DECREMENT BYTE COUNTER
          BX6    X6+X3             ADD SOURCE BYTE TO TARGET WORD
          GT     B5,B0,STOCH1      LOOP UNLESS TARGET WORD IS FULL
          SA6    A0                STORE TARGET WORD
          SB5    B5+B2             RESET BIT POSITION (ADD 60)
          SA0    A0+B1             INCREMENT TARGET ADDRESS
          LX4    B5,X2             POSITION SOURCE BYTE (CIRCULARLY)
          BX6    X4-X3             STORE BOTTOM OF SOURCE BYTE IN TARGET
          EQ     STOCH1            LOOP
          SPACE  2
**        END OF LOOP - STORE INCOMPLETE TARGET WORD, IF ANY,
*         AND RETURN
          SPACE  2
 STOCH3   SB7    B5+B4             BIT POSITION OF LAST BYTE
          EQ     B7,B2,STOCH4       RETURN IF TARGET WORD IS EMPTY
          SB3    B2-B7
          SB7    B3-B1
          MX7    1
          AX7    B7,X7              MASK FOR LAST TARGET WORD
          SA2    A0                 GET LAST TARGET WORD
          BX3    -X7*X2
          BX6    X6+X3              JOIN TARGET WORD WITH REST OF BYTE
          SA6    A0                 STORE LAST TARGET WORD
 STOCH4   SA4    STOCHA0
          SA0    X4                RESTORE OLD A0
          EQ     STOASC             RETURN
          SPACE  2
          END
#endif
