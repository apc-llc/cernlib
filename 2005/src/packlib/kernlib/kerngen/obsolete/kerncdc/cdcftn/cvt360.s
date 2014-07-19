*
* $Id: cvt360.s,v 1.1.1.1 1996/02/15 17:51:19 mclareni Exp $
*
* $Log: cvt360.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:19  mclareni
* Kernlib
*
*
          IDENT       CVT360
*
* CERN PROGLIB# M219    CVT360          .VERSION KERNCDC  2.01  820727
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
          ENTRY       CVT360
* SUBROUTINE CVT360(AI,AJ,MODE)
 TRACE    VFD         36/6HCVT360,24/3
          ENTFTN CVT360,3,X2
          SX2    X2-2
          NG          X2,ERROR         .BRANCH TO ERROR IF LT. 2 PARAMS
          ZR          X2,FLPT          .BRANCH TO FLPT IF 2 PARAMS
          SA3         B3               .PUT 3D PARAM IN X3
          ZR          X3,FLPT          .BRANCH TO FLPT MODE = 0
 INT      SA2         B1               .PICK UP NUMBER IN X2
          SB4         B0               .SET INTEGER INDICATOR
          LX2         28               .SHIFT SIGN BIT TO LEFT
          BX3         X2               .EQUIVALENCE X2 X3
          AX3         59               .FORM A WORD OF SIGN BITS
          AX2         28               .SHIFT INTEGER BACK TO RIGHT WITH
          MX0         59               .SET MASK
          BX3         -X0*X3           .SET B1 = 0 OR 1 ACC. TO SIGN
          IX6         X2-X3            .SUBTRACT 1 IF NEG. 2,S COMPTO 1,
          EQ          B0,B0,ENSTOR     .BRANCH AND STORE THE RESULT
 FLPT     SA2         B1               .SET X2 = NEXT IBM NUMBER
          SB4         B0+1             .SET FLPT INDICATOR
          BX3         X2
          MX0         36
          BX2         -X0*X2           .MASK MANTISSA
          BX3         X0*X3            .MASK EXPONENT
          LX3         28               .LEFT ADJUST EXPONENT
          BX4         X3               .EQUIVALENCE X4 AND X3
          AX4         59               .MAKE UP A WORD OF SIGN BITS
          AX3         52               .RIGHT ADJUST
          MX0         53               .SET MASK
          BX3         -X0*X3           .ENSURE X3 CONTAINS ONLY 7 BITS
          SX3         X3-64            .SUBTRACT64 BIAS
          LX3         2                .MULTIPLY BY 4 TOGO TO BASE 2
          SB5         X3-24            .SUBTRACT 24 TO CONVERT FRACT
          PX6         X2,B5            .PACK AS A FLPT NUMBER
          NX6         X6,B5            .NORMALISE THE NUMBER
          BX6         X4-X6            .COMPLEMENT IF NEGATIVE
 ENSTOR   SA6         B1
          SB1         B1+1             .ADD 1 TO FIRST ADDRESS
          LT          B2,B1,CVT360
          EQ          B4,B0,INT
          EQ          B0,B0,FLPT
 ERROR    NO
 RET      EQ          CVT360           .RETURN
          END
