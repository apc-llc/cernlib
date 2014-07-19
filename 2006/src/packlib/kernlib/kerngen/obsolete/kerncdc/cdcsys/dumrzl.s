*
* $Id: dumrzl.s,v 1.1.1.1 1996/02/15 17:51:33 mclareni Exp $
*
* $Log: dumrzl.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:33  mclareni
* Kernlib
*
*
          IDENT DUMRZL
*
* CERN PROGLIB# N202    DUMRZL          .VERSION KERNCDC  1.02  771015
*
          ENTRY DUMRZL,RESRZL,STORZL
          EXT   DUMRZLX
*
*
*     CALL DUMRZL
*     CALL STORZL
*     CALL RESRZL
*
*         PROGRAM TAKEN FROM CERN PROGLIB  15-OCT-77
*
 NAME2    VFD   36/6HSTORZL,24/STORZL
 STORZL   BSS   1                  STORE AND RESTORE REGISTER CONT.
          RJ    STOR
          SA2   STORZL             STORE RETURN ADDRESS AT DUMRZL
          BX6   X2
          SA6   DUMRZL
          EQ    B0,B0,JUMP
 NAME     VFD   36/6HDUMRZL,24/DUMRZL
 DUMRZL   BSS   1
          RJ    STOR
          SA2   DUMRZL
          AX2   30
          MX1   42
          BX7   -X1*X2             CA(1)=LOC. DUMRZL  CALLED FROM
          SX7   X7-1
          SA7   CA
          SB1   B0-7               FILL CA
          SB2   SA+7
          SB3   CA+7
 LOOP     SB1   B1+1
          SA2   B2+B1
          SA2   X2
          BX6   X2
          SA6   B3+B1
          NE    B0,B1,LOOP
          SX6   SA                 SET UP CALLING SEQUENCE
          SA6   ARG
          SX7   SB
          SA7   ARG+1
          SX6   SX
          SA6   ARG+2
          SX7   CA
          SA7   ARG+3
          SA1   ARG                ADD. FIRST ARG.
*
+RJDUMRZLX
 -        LT    B0,B4,NAME
          EQ    B0,B0,JUMP
 NAME1    VFD   36/6HRESRZL,24/RESRZL
 RESRZL   BSS   1                  RESTORE REGISTER CONTENTS
          SA2   RESRZL             STORE RETURN ADDRESS AT DUMRZL
          BX6   X2
          SA6   DUMRZL
 JUMP     MX0   18                 PREPARE THE RESTOR OF X5 AND
          SA5   SX+5               CONTENTS OF X5
          SB2   B0
          SA2   BREG
          SA1   MASK               B1 - B7
          BX6   -X1*X2             PRESET TO CORRECT SHIFT COUNT
          PL    X5,*+1
          BX6   X6+X1
          UX5   X5,B6
          SB3   7
          NO
          LX0   33
          SB4   15
 LOOP2    SA6   BREG+B2            PLUG B-REGISTER CONTENTS
          SB2   B2+1
          SA1   SB+B2
          SA2   BREG+B2
          LX3   X1,B4
          BX4   -X0*X2             CLEAR OUT
          BX7   X0*X3              RESTRICT TO 18 BITS
          IX6   X7+X4              INSERT
          NE    B2,B3,LOOP2
          LX0   15                 ADJUST FOR B7
          BX4   -X0*X2
          SA1   SA                 CONTENTS OF A0
          LX7   15
          IX6   X7+X4
          SA3   SX                 CONTENTS OF X0
          MX4   60
          BX0   X3                 X0
          SA6   A2
          SB7   X4+777777B     B7= (-0)
          LX5   11
          SA0   X1+B7              A0
          UX5   X5,B5
          SA1   SA+6               CONTENTS OF A6
          SA3   X1+B7              AND WHAT IT STORED
          LX5   11
          SA2   SA+7               CONTENTS OF A7
          SA4   X2+B7              AND WHAT IT STORED
          BX6   X3
          SA3   SA+1               CONTENTS OF A1
          BX7   X4
          SA6   X1+B7              A6
          UX5   X5,B4
          SA7   X2+B7              A7
          LX5   11
          SA2   SX+6               CONTENTS OF X6
          SA1   X3+B7              A1
          BX6   X2                 X6
          UX5   X5,B3
          SA3   SX+7               CONTENTS OF X7
          SA4   SX+1               CONTENTS OF X1
          LX5   11
          BX7   X3                 X7
          SA2   SA+2               CONTENTS OF A2
          UX5   X5,B2
          BX1   X4                 X1
          SA3   SX+2               CONTENTS OF X2
          LX5   11
          SA2   X2+B7              A2
          SA4   SA+3               CONTENTS OF A3
          UX5   X5,B1
          BX2   X3                 X2
          SA3   X4+B7              A3
          SA5   SX+3               CONTENTS OF X3
          NO
          SA4   SA+4               CONTENTS OF A4
          BX3   X5                 X3
          SA5   SX+4               CONTENTS OF X4
          SA4   X4+B7              A4
          NO
          BX4   X5                 X4
          SA5   SA+5               CONTENTS OF A5
 BREG     SA5   X5+B7              A5
          NO
          NO
          MX5   0
          PX5   X5,B1              SEQUENCE TO RESTOR B1-B7 AND X5
          SB1   B7+0
          LX5   49
          PX5   X5,B2
          SB2   B7+0
          LX5   49
          PX5   X5,B3
          SB3   B7+0
          LX5   49
          PX5   X5,B4
          SB4   B7+0
          LX5   49
          PX5   X5,B5
          SB5   B7+0
          LX5   49
          PX5   X5,B6
          SB6   B7+0
          SB7   B7+0
          EQ    B0,B0,DUMRZL
 STOR     PS
 +        PL    B7,*+2             BIT 17 OF B7   STORE REGISTERS
          RJ    *+1
 W7       DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 16
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 15
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 14
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 13
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 12
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 11
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 10
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 9
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 8
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 7
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 6
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 5
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 4
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 3
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 2
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 1
          RJ    *+1
          DATA  0
          SB7   B7+B7
 +        PL    B7,*+2             BIT 0
          RJ    *+1
          DATA  0
          SB7   A7
          SA7   SX+7               X7
          SX7   A6
          SA6   SX+6               X6
          SA7   SA+6               A6
          BX6   X0
          LX7   X1,B0
          SA6   SX                 X0
          SA7   SX+1               X1
          BX6   X2
          LX7   X3,B0
          SA6   SX+2               X2
          SA7   SX+3               X3
          BX6   X4
          LX7   X5,B0
          SA6   SX+4               X4
          SA7   SX+5               X5
          SX6   A0-B0
          SX7   A1-B0
          SA6   SA                 A0
          SA7   SA+1               A1
          SX6   A2-B0
          SX7   A3-B0
          SA6   SA+2               A2
          SA7   SA+3               A3
          SX6   A4-B0
          SX7   A5-B0
          SA6   SA+4               A4
          SA7   SA+5               A5
          SX6   B7-B0
          SX7   B1-B0
          SA6   SA+7               A7
          SA7   SB+1               B1
          SX6   B2-B0
          SX7   B3-B0
          SA6   SB+2               B2
          SA7   SB+3               B3
          SX6   B4-B0
          SX7   B5-B0
          SA6   SB+4               B4
          SA7   SB+5               B5
          SX6   B6-B0
          MX2   0
          SA6   SB+6               B6
          BX7   X2
          MX4   1
          SA3   W7                 NOW RECONSTRUCT B7
          SB1   1
          SB2   17
          SB3   3
 LOOP1    ZR    X3,*+1
          SA7   A3                 RESET TO ZERO
          BX2   X4+X2
          SB2   B2-B1
          NO
          SA3   A3+B3
          LX2   1
          PL    B2,LOOP1
          BX6   X2
          MX7   0
          SA6   SB+7               B7
          SA7   SB                 B0
          EQ    B0,B0,STOR
 MASK     DATA  74B
 CA       BSS   8
 SA       BSSZ  8
 SB       BSSZ  8
 SX       BSSZ  8
 ARG      BSSZ  5                  PARAM. ADDRESSES FOR PRIRZL
          END
