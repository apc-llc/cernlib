*
* $Id: sortzv.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: sortzv.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT  SORTZV
*
* CERN PROGLIB# M101    SORTZV          .VERSION KERNCDC  1.16  820129
*
* SUBROUTINE SORTZV MODIFIED 23/07/79 BY H.R.RENSHALL/DD/US
*     TO MAKE CODE COMPATIBLE WITH DOCUMENTATION THAT IMPLIES THE 7 TH
*     AND 8 TH PARAMETERS MAY BE LEFT OUT OF THE CALL WHEN CHARACTER
*     SORTING. THIS RESULTS IN WORDS 0 AND 1 BEING OVERWRITTEN.
*     LENGTH OF CORRECTION IS TO CATER FOR REPEATED CALLS OF SORTZV
*     WITH DIFFERENT NUMBERS OF PARAMETERS.
*
 ENTFTN   MACRO     (NM,NOPAR,REG)
          LOCAL     Z
 NM       BSSZ      1
 Z1Z1Z    IFNE      *F,1
          EQ        *+3
          BSSZ      2
          SA2       NM
          SA3       =O5302046000
          BX6       X2+X3
          LX6       30
          SA6       NM+2
          SA3       =O51200000000400000000
          SX4       NM+2
          IX3       X3+X4
          SX4       X4+1
          LX4       30
          BX6       X3+X4
          SA6       A2
          SX6       A0
          SB7       A1
          SA6       NM+3
 Z1Z1     SET       1                   DIGITSELECTOR
          DUP       NOPAR
          ZR        X1,Z
 Z1Z1Z1   IFLT      Z1Z1,7              IF FIRST SIX
          SB.Z1Z1   X1
 Z1Z1Z1   ELSE     2
          LX6       X1
          SA6       NM-NOPAR-2+Z1Z1
 Z1Z1Z1   ENDIF
 Z1Z1     SET       Z1Z1+1
 Z1Z      IFGE      NOPAR,Z1Z1
          SA1       A1+1                LOAD NEXT
 Z1Z      ENDIF
          ENDD
 Z        BSS       0
Z1Z1Z11   IFC       NE,/REG//
Z1Z1      SET    1
Z1Z1Z1Z   IFC    EQ,/REG/X1/
Z1Z1      SET    2
          BX2    X1
Z1Z1Z1Z   ENDIF
          S_REG     A1-B7
          IFNE      NOPAR,0
          ZR    X.Z1Z1,*+2-$/46
          S_REG     REG+1
          BSS       0
          ENDIF
          ENDM
          ENTRY       SORTZV
* SUBROUTINE SORTZV(A,INDEX,N,MODE,NWAY,NSORT,M,CARSET)
*
*     CORRECTIONS OF 23/07/79. DEFINE ZERO AND ITS ADDRESS.
ZERO      DATA        0                .UNCHANGED ZERO 23/07/79
*     END OF CORRECTIONS.
 M        DATA        0                 .ADDRESS OF M
 CARSET   DATA        0                 .ADDRESS OF CARSET
 +        VFD         36/0HSORTZV,24/8
*
*     CORRECTIONS OF 23/07/79. GET THE ACTUAL NUMBER OF PARAMETERS IN X5
*     AND IF LESS THAN 8 LOAD THE ADDRESS OF A WORD CONTAINING ZERO
*     INTO THE LOCATIONS OF M AND CARSET. THIS MUST BE DONE EACH TIME
*     AS THESE ADDRESSES ARE REPLACED BY VALUES AND NOT REFRESHED IN A
*     CALL TO SORTZV WITH LESS PARAMETERS THAN 8.
          ENTFTN       SORTZV,8,X5     .GET ACTUAL NO PARMS IN X5
          SX4         7B               .WILL TEST AGAINST GT 7 PARMS
          IX5         X5-X4            .SUBTRACT 7 FROM ACTUAL NUMBER
          PL          X5,BEGIN         .IF POSITIVE BYPASS FIXUP
          SX6         ZERO             .PUT ADDRESS OF ZERO INTO X6
          SA6         M                .LOAD ADDRESS OF ZERO INTO M
          SA6         CARSET           .LOAD ADDRESS OF ZERO INTO CARSET
*     M NOW CONTAINS A VALID ADDRESS ALWAYS.
*     END OF CORRECTIONS.
BEGIN     MX0         59               .X0=-1 LABEL BEGIN ADDED
          SA1         B6               .LOAD NSORT
          SA2         B3               .LOAD N
          MX6         0                .X6=0
          SB6         B2+X0            .ADDRESS INDEX(0)
          SB7         X2               .B7=N
          NZ          X1,FILLED        .ARRAY INDEX IS PRESET
 LOOP0    IX6         X6-X0            .X6=X6+1
          SB7         B7-1
          SA6         B6+X6            .PRESET FIRST N OF INDEX
          LT          B0,B7,LOOP0
          SB7         X2
 FILLED   SA3         B5               .LOAD WAY
          SA1         B4               .LOAD MODE
          BX6         X2               .SAVE N
          LX7         X3,B0            .SAVE WAY
          SA6         N
          SA7         WAY
          ZR          X1,CHARAC        .CHARACTER SORT WANTED
          PL          X1,FLOAT         .FLOATING SORT WANTED
          SA1         INTSOR1          .MODIFIERS
          SA3         INTSOR2          .
          EQ          B0,B0,START
 FLOAT    SA1         FLPSOR1
          SA3         FLPSOR2
 START    BX6         X1
          LX7         X3,B0
          SB1         B1+X0            .(A(0))
          SB2         B2+X0            .(INDEX(0))
          SA6         LOOP1
          SA7         ALTER+1
          RJ          *+1
          BSS         1
          BX6         -X0              .X6=1
          LX7         X2,B0            .X7=N
          SB5         X6               .B5=1
          SB6         B5+B5            .B6=2
          SA6         LL               .STORE BOUNDARIES
          SA7         LU
          SA0         A6-B5            .LL(0)
          SB7         A7-B5            .LU(0)
 LOC0     SB6         B6-B5            .P=P-1
          SA1         A0+B6            .LOWER
          SA2         B6+B7            .UPPER
          IX0         X1-X2
          ZR          B6,TERMIN
          PL          X0,LOC0          .NOT MEANINGFUL
          BX6         X1
          LX7         X2,B0
          SB3         X1+B5
          SB4         X2
          SA6         ILOW
          SA7      A6+B5
          IX0         X6+X7
          AX1         X0,B5
          SA2         X6+B2
          SA4         X1+B2            .LOAD MIDDLE
          BX7      X1
          SA5         X4+B1            .LOAD VALUE
          BX6         X2
          SA7      A6-B5      MIDDLE
          SA6         A4               .INSERT LOWEST TO MIDDLE
 LOOP1    SA1         B2+B3            .GET INDEX OF NEXT ITEM
          SA2         X1+B1            .GET VALUE
          RX0         X5-X2
 +        SB3         B3+B5            .INCREASE POINTER
          NO
          NG          X0,ALTER         .SEQUENCE BROKEN
 TEST     GE          B4,B3,LOOP1
 LOC1     SA3      MIDDLE
          SA2      A3+B5      ILOW
          BX6      X4         INDEX OF T
          LX7      X1,B0      INDEX OF UPPERMOST IN LOWER
          SB3      X3
          SA6      A1         STORE INTO FINAL LOCATION
          SA7      X2+B2      STORE INTO LOWEST OF LOWER
          SA3      A2+B5      IUPP
          LT       B3,B4,LOC2
          SX6      B4+B5      UPPER INTERVAL IS LARGER
          BX7      X3
          LX1      X2,B0      NEXT ILOW
          SX2      B4-B5      NEXT IUPP
          SA6      A0+B6      LL(P)
          SA7      B7+B6      LU(P)
          SB6      B6+B5      P=P+1
          IX0      X1-X2
          JP       LOC0+1
 LOC2     SX7      B4-B5      LOWER INTERVAL IS LARGER
          BX6      X2
          LX2      X3,B0      NEXT IUPP
          SX1      B4+B5      NEXT ILOW
          SA6      A0+B6      LL(P)
          SA7      B7+B6      LU(P)
          SB6      B6+B5      P=P+1
          IX0      X1-X2
          JP       LOC0+1
 ALTER    LT          B4,B3,LOC3       .SEARCH IN UPPER INETRVAL FOR
          SA2         B4+B2            .SMALLER ITEM TO SWAP
          SA3         X2+B1
          RX0         X3-X5
          SB4         B4-B5
          PL          X0,ALTER
          BX6         X1               .PRPARE SWAP
          LX7         X2,B0
          SA6         A2               .SWAP
          SA7         A1
          NO
          SA1         B4+B2            .RELOAD FOR CASE B4.LT.B3
          EQ          B0,B0,TEST
 LOC3     SX0         B4-B5            .UPPERMOST IN LOWER.B4 TO FIRST U
          SA2         ILOW
          IX0         X2-X0
          SB3         B4               .LOWEST IN UPPER
          SB4         B4-B5
          SA1         B4+B2            .UPPERMOST IN LOWER
          NZ          X0,LOC1
          BX1         X4               .REPLACE BY ITSELF
          EQ          B0,B0,LOC1
 TERMIN   SA1         WAY
          SA3         N
          ZR          X1,SORTZV        .ASCENDING WANTED .DONE
          SB1         B5               .
          SB6         X3               .SWAP ARRAY INDEX TO BE
          AX3         1                .DESCENDING
          SX4         B5               .X4=1
          GE          B5,B6,SORTZV     .N IS NONSENSE OR ONE
 LOOP10   SA1         B2+B1
          SA2         B2+B6
          BX6         X1
          LX7         X2,B0
          IX3         X3-X4
          SB6         B6-B5
          SB1         B1+B5
          NO
          SA6         A2
          SA7         A1
          NZ          X3,LOOP10
          EQ          B0,B0,SORTZV
 N        DATA        0
 WAY      DATA        0
 INTSOR1  SA1         B2+B3            .PLUG IN INSTRUCTIONS
          SA2         X1+B1
          IX0         X5-X2
 FLPSOR1  SA1         B2+B3
          SA2         X1+B1
          FX0         X5-X2
 INTSOR2  IX0         X3-X5
          SB4         B4-B5
          PL          X0,ALTER
 FLPSOR2  FX0         X3-X5
          SB4         B4-B5
          PL          X0,ALTER         .END OF PLUG IN
 CHARAC   SA1         M                .TO SEE WHICH CHARACTERSET IS TO
          SA2         CARSET           .BE USED
          SA3         X1               .LOAD VALUE M
          SB4         X2               .ADDRESS OF CARSET
          MX5         54
          SB1         B1+X0            .ADDRESS OF A(0)
          BX7         X3
          SB2         B2+X0            .ADDRESS OF INSEX(0)
          SB3         X0               .B3=-1
          SA7         A1               .REPLACE ADDRESS M BY VALUE
          GE          B0,B7,SORTZV
          NZ          X3,TABLE         .USER HAS OWN CHARACTERSET
 NORM     SB7         10               .SWAP 00 AND 55
          SX3         55B
          SB4         STARTC           .NEXT AFTER CONVERSION
          SB6         6
 LOOP2    SA1         B2+X6            .X6=N
          SA2         B1+X1            .LOAD DATA WORD
          IX6         X6+X0            .BUMP DOWN
          SB5         B7
 LOOP3    LX2         X2,B6
          BX1         -X5*X2           .GET CHARACTER
          IX4         X1-X3            .IS IT A BLANK
          SB5         B5+B3
 +        NZ          X1,*+1           .NOT A 00
          BX2         X2+X3            .INSERT 55
 +        NZ          X4,*+1           .NOT A 55
          BX2         X5*X2            .CLEAR TO ZERO
 +        NZ          B5,LOOP3
          BX7         X2
          NO
          NO
          SA7         A2               .STORE BACK
          NZ          X6,LOOP2
          SB5         1
          JP          B4               .ALLOWS TO USE FOR RECONVERTION
 STARTC   SA1         N
          BX6         -X0
          LX7         X1,B0
          SA6         LL
          SA7         LU
          SB5         X6               .B5=1
          SA0         A6-B5            .LL(0)
          SB7         A7-B5            .LU(0)
          SB6         B5+B5
 LOC01    SB6         B6-B5            .P=P-1
          SA1         A0+B6            .LOWER
          SA2         B6+B7            .UPPER
          IX0         X1-X2
          ZR          B6,TERMINC
          PL          X0,LOC01         .NOT MEANINGFUL
          BX6         X1
          LX7         X2,B0
          SB3         X1+B5
          SB4         X2
          SA6         ILOW
          SA7      A6+B5
          IX0         X6+X7
          AX1         X0,B5
          SA2         X6+B2
          SA4         X1+B2            .LOAD MIDDLE
          BX7      X1
          SA5         X4+B1            .LOAD VALUE
          BX6         X2
          SA7      A6-B5      MIDDLE
          SA6         A4               .INSERT LOWEST TO MIDDLE
 BRANCH   NG          X5,NEG
          SA1         PLUS
          BX6         X1
          LX7         X1,B0
          EQ          B0,B0,CONT
 NEG      SA1         NEGATIV
          SA2         NEGAT2
          BX6         X1
          LX7         X2,B0
 CONT     SA6         FIRST
          SA7         SECOND
          RJ          *+1
          BSS         1
 LOOP11   SA1         B2+B3            .GET INDEX OF NEXT ITEM
          SA2         X1+B1            .GET VALUE
          IX0         X5-X2
          SB3         B3+B5            .INCREASE POINTER
 FIRST    BSS         1                .PLUGWORD
 TESTC    GE          B4,B3,LOOP11
 LOC11    SA3      MIDDLE
          SA2      A3+B5      ILOW
          BX6      X4         INDEX OF T
          LX7      X1,B0      INDEX OF UPPERMOST IN LOWER
          SB3      X3
          SA6      A1         STORE INTO FINAL LOCATION
          SA7      X2+B2      STORE INTO LOWEST OF LOWER
          SA3      A2+B5      IUPP
          LT       B3,B4,LOC21
          SX6      B4+B5      UPPER INTERVAL IS LARGER
          BX7      X3
          LX1      X2,B0      NEXT ILOW
          SX2      B4-B5      NEXT IUPP
          SA6      A0+B6      LL(P)
          SA7      B7+B6      LU(P)
          SB6      B6+B5      P=P+1
          IX0      X1-X2
          JP       LOC01+1
 LOC21    SX7      B4-B5      LOWER INTERVAL IS LARGER
          BX6      X2
          LX2      X3,B0      NEXT IUPP
          SX1      B4+B5      NEXT ILOW
          SA6      A0+B6      LL(P)
          SA7      B7+B6      LU(P)
          SB6      B6+B5      P=P+1
          IX0      X1-X2
          JP       LOC01+1
 ALTERC   LT          B4,B3,LOC31      .SEARCH IN UPPER INETRVAL FOR
          SA3         B4+B2            .SMALLER ITEM TO SWAP
          SA2         X3+B1
          IX0         X5-X2
          SB4         B4-B5
 SECOND   BSS         1                .PLUGWORD
          BX6         X1               .PRPARE SWAP
          LX7         X3,B0
          SA6         A3               .SWAP
          SA7         A1
          NO
          SA1         B4+B2            .RELOAD FOR CASE B4.LT.B3
          EQ          B0,B0,TESTC
 LOC31    SX0         B4-B5            .UPPERMOST IN LOWER.B4 TO FIRST U
          SA2         ILOW
          IX0         X2-X0
          SB3         B4               .LOWEST IN UPPER
          SB4         B4-B5
          SA1         B4+B2            .UPPERMOST IN LOWER
          NZ          X0,LOC11
          BX1         X4               .REPLACE BY ITSELF
          EQ          B0,B0,LOC11
 TERMINC  SA1         M
          SA2         N
          SB7         10
          SX3         55B
          SB6         6
          BX6         X2
          SB4         TERMIN
          MX0         59
          SB3         X0
          MX5         54
          ZR          X1,LOOP2
          MX5         6
          SB5         6
          EQ          B0,B0,CONVERT
 TABLE    MX7         59               .USER HAS OWN TABLE NOW BUILT UP
          SB5         77B              .A TWO WAY TABLE AND CHECK FOR
          SB7         X6               .COMPLETENESS. B7=N
 LOOP20   SA1         B4+B5            .
          BX6         X5*X1
          SB5         B5+B3
          SA7         TICK+1+B5        .SET UP A TICK OFF TABLE
          SA6         A1
          PL          B5,LOOP20
          MX5         6                .B5=-1 NOW
          SB6         X3+B3            .NUMBER OF CHARACTERS SPECIFIED-1
 LOOP21   SB5         B5-B3
          SA1         B4+B5
          BX2         X5*X1            .EXTRACT CHARACTER
          IX7         X7-X0
          LX2         6
          SA4         X2+B4
          BX6         X7+X4
          SA7         TICK+X2
          SA6         A4
          LT          B5,B6,LOOP21
          SB3         100B
          SB5         B0
          BX0         -X0
          SA1         TICK+B5
 LOOP22   PL          X1,DOWN          .CHARACTER IS SPECIFIED
          SA2         B4+B5            .PLACE OF POINTER
          IX7         X7+X0
          BX6         X2+X7
          SB6         B6+X0
          SX4         B5
          SA6         A2
          GE          B6,B3,MAD
          SA3         B4+B6
          BX2         -X5*X3
          LX4         54
          BX6         X4+X2
          NO
          SA6         A3
 DOWN     SB5         B5+X0
          SA1         A1+1
          LT          B5,B3,LOOP22
          SB4         STARTC
          MX4         54
          BX5         -X4
          SB5         B0
 CONVERT  SA1         N                .CONVERT NWORDS
          SA2         CARSET
          SB6         1
          SB7         X2
          MX4         54
          BX0         X1
 LOOP23   SA1         B2+X0
          SA2         B1+X1
          SB3         10
 LOOP24   LX2         6
          BX3         -X4*X2           .GET CHARACTER
          SA1         B7+X3
          BX6         X5*X1
          LX7         X6,B5            .ADJUST NEW CHARACTER
          SB3         B3-B6
          BX3         X4*X2
          IX2         X3+X7
          NZ          B3,LOOP24
          BX6         X2
          SX0         X0-1
          SA6         A2
          NZ          X0,LOOP23
          MX0         59
          SB5         B6
          JP          B4
 MAD      SX6         B7               .X6=N
          SX7         B0
          MX5         54
          BX0         -X0
          SA7         M
          SB3         X0
          EQ          B0,B0,NORM
 PLUS     NG          X2,ALTERC
          NG          X0,ALTERC
 NEGATIV  PL          X2,TESTC
          NG          X0,ALTERC
 NEGAT2   PL          X2,SECOND+1
          NG          X0,ALTERC
 MIDDLE   BSS      1
 ILOW     BSS         1
 IUPP     BSS         1
 LL       BSS         40
 LU       BSS         40
 TICK     EQU         LL
          END
#ifdef CERNLIB_TCGEN_SORTZV
#undef CERNLIB_TCGEN_SORTZV
#endif
