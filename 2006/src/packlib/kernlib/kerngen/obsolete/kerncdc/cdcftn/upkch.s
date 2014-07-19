*
* $Id: upkch.s,v 1.1.1.1 1996/02/15 17:51:26 mclareni Exp $
*
* $Log: upkch.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:26  mclareni
* Kernlib
*
*
          IDENT UPKCH
*
* CERN PROGLIB# M427    UPKCH           .VERSION KERNCDC  0.1   730123
*
          ENTRY UPKCH
*
*     CALL UPKCH (CHAR,INT,N,IPAR)  IPAR=NBITS,NCHAR,NZONE,IGNOR
*
*         REGISTER ASSIGNMENTS
*
*              A0  ADR(INT(J))
*              X7  INT(J)                  B7  =1
*              X6                          B6  NBITS
*              X5                          B5  NBITSW PER WORD
*              X4  NSKIP (SEC 2)           B4  NCHAR
*              X3                          B3  JGO (SEC 1)
*              X2  -N, COUNT UP            B2  CHARACTER COUNTER
*              X1  WORD BEING UNPACKED     B1  JR = 60-JBT  ROTATOR
*              X0  MASK OF NBITS BITS
*
          VFD   36/6HUPKCH ,24/UPKCH    *BL
 SAVEA0   DATA  0
 UPKCH    DATA  0
          SB7   1              B7= 1
          SA2   A1+B7
          SA3   A2+B7
          SA4   A3+B7
          SX6   A0
          SB1   X1
          SB2   X2
          SB3   X3
          SA6   SAVEA0
          SA3   X4
          SA1   A3+B7          X1= NCHAR
          SA5   A1+B7         *X5= NZONE
          SB5   60             B5= NBITSW
          SA4   A5+B7          X4= IGNOR
          PX0   X3
          NZ    X5,PREL11
          SX5   B5                 NZONE = NBITSW
 PREL11   BSS   0
          NX0   B6,X0
          IX7   X5-X4          X7= NZONE - IGNOR
          PX6   X7                 MXCHAR= (NZONE-IGNOR)/NBITS
          SA0   B2-B7          A0= ADR (INT(0))
          FX6   X6/X0
          SA2   B3             X2= N
          SB6   X3             B6= NBITS
          ZR    X2,EXITZ
          BX2   -X2            X2= -N
          SB2   X4            *B2= IGNOR
          UX6   B3,X6
          LX6   B3,X6          X1= NCHAR = MXCHAR
          SB3   B6-B7          B3= NBITS-1
          NZ    X1,PREL31
 PREL21   BX1   X6
 PREL31   DX4   X1*X3         *X4= NCHAR*NBITS
          IX0   X7-X4          X0= NTRAIL = NZONE - IGNOR - NCHAR*NBITS
          SB4   X1             B4= NCHAR
          NG    X0,PREL21          ORIGINAL NCHAR TOO BIG
          MX0   1
          SX6   B1
          AX0   B3,X0              GENERATE NBITS BITS
          SB3   X5
          SA1   B1             A1= ADR(CHAR(1))
          LX0   B6,X0          X0= MASK
          SA6   NCH                NCH = ADR(CHAR(1))
          SB1   B2+B6          B1= JR = IGNOR + NBITS
          NE    B3,B5,ZONED
*
*--------      SECTION 1  =    ZONE EQUALS WORD
*
          SB3   B1             B3= JGO = JR INITIAL
          EQ    GO1
*
*              LOOP 1
*
 ST24L1   LX6   B1,X1              POSITION BYTE
          SA0   A0+B7
          SB2   B2+B7              COUNT UP
          BX7   X0*X6              PICK INT
          SB1   B1+B6              JR = JR + NBITS
          SA7   A0                 STORE INT
          LT    B2,B4,ST24L1       CHECK CHAR COUNT
          PL    X2,EXIT
          SA1   A1+B7              LOAD NEXT PACKED WORD
          SB1   B3                 JBT= JGO
 GO1      SX2   X2+B4              -N= -N + NCHAR
          SB2   B0                 INITIAL CHAR COUNT
          NG    X2,ST24L1
          SB2   X2
          EQ    ST24L1
          EJECT
*
*--------      SECTION 2  =    ZONE NOT EQUALS WORD
*
 ZONED    IX4   X5-X4          X4= NSKIP = NZONE - NCHAR*NBIT
          BX3   X1             X3= WORKING COPY OF X1, WORD FOR UNPK
          NZ    X4,ACT4
          SB3   B5+B7          B3= NBITSW+1
          EQ    JOIN3
 ACT4     SB3   B5+B6          B3= NBITSW + NBITS
          SX2   X2+B4              -N= -N + B4
          SB2   B0
          NG    X2,ST43L4
          SB2   X2
          EQ    ST43L4
*
*              LOOP 3      SPECIAL FOR CHARCTERS COMPACT
*
 PART3    AX6   B1,X0              MASK OF NLEFT = NBITS-NRIGHT  BITS
          BX7   X6*X3              PICK LEFT BITS IN OLD X1
          BX5   -X6*X1             CLEAR LEFT BITS IN NEW X1
          BX3   X5+X7              JOIN OLD+NEW
 JOIN3    LX5   B1,X3              POSITION BYTE
          SA0   A0+B7
          SX2   X2+B7              BUMP UP
          BX7   X0*X5              PICK INT
          NO
          SB1   B1+B6              JR = JR+NBITS
          SA7   A0                 STORE INT
          BX3   X1
          PL    X2,EXIT
          LT    B1,B3,JOIN3
          SA1   A1+B7              FETCH NEW PACKED WORD
          SB1   B1-B5              JR = JR-60  = NRIGHT
          EQ    PART3
*
*              LOOP 4      GENERAL
*
 PART4    AX6   B1,X0              MASK OF NLEFT = NBITS-NRIGHT  BITS
          BX7   X6*X3              PICK LEFT BITS IN OLD X1
          BX5   -X6*X1             CLEAR LEFT BITS IN NEW X1
          BX3   X5+X7              JOIN OLD+NEW
 JOIN4    LX5   B1,X3              POSITION BYTE
          SA0   A0+B7
          SB2   B2+B7              BUMP UP
          BX7   X0*X5              PICK INT
          NO
          SB1   B1+B6              JR = JR+NBITS
          SA7   A0                 STORE INT
          BX3   X1
 ST44L4   GE    B2,B4,ENDZ
          LT    B1,B5,JOIN4
          SA1   A1+B7              FETCH NEW PACKED WORD
          SB1   B1-B5              JR = JR-60  = NRIGHT
          EQ    PART4
 ENDZ     PL    X2,EXITCH
          SX2   X2+B4              -N = -N + NCHAR
          SB1   X4+B1              JR = JR+NSKIP
          SB2   B0                 CHAR COUNT INIT
          NG    X2,ST43L4
          SB2   X2
 ST43L4   LT    B1,B3,ST44L4
          SA1   A1+B7              FETCH NEW PACKED WORD
          SB1   B1-B5              JR = JR-NBITSW= JR-60
          BX3   X1
          EQ    ST43L4
*
 EXITZ    MX6   0
          SA5   SAVEA0
          SA6   NCH
          SA0   X5
          EQ    UPKCH
 EXITCH   NE    B1,B6,EXIT
          SA1   A1-B7
 EXIT     SA3   NCH
          SX2   A1+B7
 SKIP     IX6   X2-X3
          SA5   SAVEA0
          SA6   NCH                SET NCH WORDS USED INTO /SLATE/
          SA0   X5
          EQ    UPKCH
*
          USE   /SLATE/
 NCH      BSS   40
          END
#ifdef CERNLIB_TCGEN_UPKCH
#undef CERNLIB_TCGEN_UPKCH
#endif
