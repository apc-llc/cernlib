*
* $Id: pkchar.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: pkchar.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT PKCHAR
*
* CERN PROGLIB# M427    PKCHAR          .VERSION KERNCDC  0.1   730123
*
          ENTRY PKCHAR
*
*     CALL PKCHAR (INT,CHAR,N,IPAR)  IPAR=NBITS,NCHAR,NZONE,IGNOR,NFILL
*
*         REGISTER ASSIGNMENTS
*
*              A0  ADR(NCHAR(J))
*              X7  WORD BEING COMPILED     B7  =1
*              X6                          B6  NBITS
*              X5                          B5  NBITSW PER WORD
*              X4  -NSKIP (SEC 2)          B4  NCHAR
*              X3  NFILL                   B3  JGO (SEC 1)
*              X2  -N, COUNT UP            B2  CHARACTER COUNTER
*              X1  INT(JIN)                B1  JBT, BIT POSITION
*              X0  MASK OF NBITS BITS
*
          EJECT
          VFD   36/6HPKCHAR,24/PKCHAR
 SAVEA0   DATA  0
 PKCHAR   DATA  0
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
          PX0   X3             X0= NBITS FLOATING
          NZ    X5,PREL11
          SX5   B5                 NZONE = NBITSW
 PREL11   BSS   0
          NX0   B6,X0
          IX7   X5-X4          X7= NZONE - IGNOR
          PX6   X7                 MXCHAR= (NZONE-IGNOR)/NBITS
          SA0   B2             A0= ADR (CHAR(1))
          FX6   X6/X0
          SA2   B3             X2= N
          SB6   X3             B6= NBITS
          ZR    X2,EXITZ
          BX2   -X2            X2= -N
          SB2   X4            *B2= IGNOR
          UX6   B3,X6
          LX6   B3,X6          X6= MXCHAR
          SB3   B6-B7          B3= NBITS-1
          NZ    X1,PREL31
 PREL21   BX1   X6
 PREL31   DX4   X1*X3         *X4= NCHAR*NBITS
          IX0   X7-X4          X0= NTRAIL = NZONE - IGNOR - NCHAR*NBITS
          SB4   X1             B4= NCHAR
          NG    X0,PREL21          ORIGINAL NCHAR TOO BIG
          SA3   A4+B7          X3= NFILL
          MX0   1
          SX6   A0
          AX0   B3,X0              GENERATE NBITS BITS
          SB3   X5             B3= NZONE
          SA1   B1             A1= ADR(INT(1))
          BX7   X3                 PRESET FIRST WORD TO NFILL
          SA6   NCH                NCH = ADR(CHAR(1))
          LX0   B6,X0          X0= MASK
          SB1   B5-B2          B1= JBT = NBITSW-IGNOR  = JGO
          BX1   X0*X1          X1= CLEANED INT(1)
          SB2   B0
          SB1   B1-B6              JBT= JBT - NBITS
          NE    B3,B5,ZONED
*
*--------      SECTION 1  =    ZONE EQUALS WORD
*
          SB3   B1             B3= JGO = JBT INITIAL
          SX2   X2+B4              -N= -N + NCHAR
          ZR    X3,GO1
          EQ    GO2
*
*              LOOP 1      SPECIAL FOR NFILL=0
*
 ST24L1   BX5   X0*X1              CLEAN INT
          SA1   A1+B7              LOAD NEXT INT
          SB2   B2+B7              COUNT UP
          LX3   B1,X5              POSITION INT
          BX7   X3+X7              SET BYTE
          SB1   B1-B6              JBT= JBT - NBITS
          LT    B2,B4,ST24L1       CHECK CHAR COUNT
          SA7   A0                 STORE COMPLETED WORD
          SA0   A0+B7
          PL    X2,EXITNS
          SB1   B3                 JBT= JGO
          MX7   0
          SX2   X2+B4              -N= -N + NCHAR
          SB2   B0                 INITIAL CHAR COUNT
 GO1      NG    X2,ST24L1
          SB2   X2
          EQ    ST24L1
*
*              LOOP 2      GENERAL
*
 ST24L2   BX5   X0*X1              CLEAN INT
          LX6   B1,X0              POSITION MASK
          SA1   A1+B7              LOAD NEXT INT
          SB2   B2+B7              COUNT UP
          BX7   -X6*X7             CLEAR TARGET BYTE
          LX5   B1,X5              POSITION INT
          SB1   B1-B6              JBT= JBT - NBITS
          BX7   X5+X7              SET BYTE
          LT    B2,B4,ST24L2
          SA7   A0                 STORE COMPLETED WORD
          SA0   A0+B7
          PL    X2,EXITNS
          SB1   B3                 JBT= JGO
          SX2   X2+B4              -N= -N + NCHAR
 GO2      BX7   X3
          SB2   B0                 INITIAL CHAR COUNT
          NG    X2,ST24L2
          SB2   X2
          EQ    ST24L2
          EJECT
*
*--------      SECTION 2  =    ZONE NOT EQUALS WORD
*
 ZONED    IX4   X4-X5          X4= -NSKIP = -(NZONE - NCHAR*NBITS)
          NZ    X4,GO4
          NZ    X3,GO4             NFILL NOT ZERO
          EQ    GO3
*
*              LOOP 3      SPECIAL FOR CHARCTERS COMPACT, NFILL = 0
*                              NRIGHT=-JBT,  NLEFT=NBITS-JBT
 PART3    BX1   -X6*X1             CLEAR NLEFT BITS OF INT
          SA0   A0+B7
          SB1   B1+B5              JBT= JBT + NBITSW
          BX7   X3                 CLEAR NEW WORD
 JOIN3    SX2   X2+B7              COUNT UP
          LX5   B1,X1              POSITION INT
          SA1   A1+B7              LOAD NEXT INT
          BX7   X5+X7              SET BYTE
          SB1   B1-B6              JBT= JBT - NBITS
          PL    X2,EXIT
          BX1   X0*X1              CLEAN INT
          GE    B1,B0,JOIN3
          LX6   B1,X1              KEEP NLEFT BITS OF INT
          BX7   X6+X7              SET PARTIAL BYTE
          AX6   B1,X6              RE-POSITION NLEFT BITS ON INT
          SA7   A0                 STORE COMPLETED WORD
          EQ    PART3              PARTIAL  BYTE
 GO3      EQU   JOIN3
*
*              LOOP 4      GENERAL
*                              NRIGHT=-JBT,  NLEFT=NBITS-JBT
 PART4    BX1   X6*X1              KEEP NRIGHT BITS
          LX6   B1,X6              POSITION MASK
          SA0   A0+B7
          BX7   X3                 PRESET NEW WORD
 JOIN4    LX5   B1,X1              POSITION INT
          SA1   A1+B7              LOAD NEXT INT
          BX7   -X6*X7             CLEAR TARGET BYTE
          SB2   B2+B7              COUNT UP
          SB1   B1-B6              JBT= JBT-NBITS
          BX7   X5+X7              SET BYTE
          GE    B2,B4,ENDZ
 ST44L4   LX6   B1,X0              POSITION MASK /  MASK OF NLEFT BITS
          BX1   X0*X1              CLEAN INT
          GE    B1,B0,JOIN4        COMPLETE BYTE
          BX7   -X6*X7             CLEAR PARTIAL TARGET BYTE
          LX5   B1,X1              KEEP NLEFT BITS OF INT
          SB3   B1+B6          B3= NLEFT
          BX7   X5+X7              SET PARTIAL BYTE
          SB1   B1+B5              JBT= JBT+NBITSW
          AX6   B3,X0              SET MASK OF NRIGHT BITS
          NO
          SA7   A0                 STORE COMPLETED WORD
          EQ    PART4              PARTIAL BYTE
 ENDZ     PL    X2,EXIT
          SB1   X4+B1              JBT= JBT - NSKIP
 GO4      SX2   X2+B4              -N= -N + NCHAR
          SB2   B0             B2= 0  COUNT CHARACTERS
          NG    X2,ST43L4
          SB2   X2
 ST43L4   SB3   B1+B6
          GE    B3,B0,ST44L4
          SA7   A0                 STORE COMPLETED WORD
          SA0   A0+B7
          SB1   B1+B5              JBT= JBT + NBITSW
          BX7   X3                 PRESET NEW WORD
          EQ    ST43L4
*
 EXITZ    MX6   0
          SA5   SAVEA0
          SA6   NCH
          SA0   X5
          EQ    PKCHAR
 EXIT     SA7   A0                 STORE LAST WORD
          SA0   A0+B7
 EXITNS   SA3   NCH
          SA5   SAVEA0
          SX2   A0
          IX6   X2-X3
          SA6   NCH                SET NCH WORDS USED INTO /SLATE/
          SA0   X5
          EQ    PKCHAR
*
          USE   /SLATE/
 NCH      BSS   40
          END
#ifdef CERNLIB_TCGEN_PKCHAR
#undef CERNLIB_TCGEN_PKCHAR
#endif
