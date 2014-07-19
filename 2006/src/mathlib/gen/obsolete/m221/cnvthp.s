*
* $Id: cnvthp.s,v 1.1.1.1 1996/04/01 15:03:20 mclareni Exp $
*
* $Log: cnvthp.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:20  mclareni
* Mathlib gen
*
*
          IDENT       CNVTHP
#include "entftn.inc"
          ENTRY       CNVTHP
* SUBROUTINE CNVTHP(AI,AJ,MODE)
* USAGE:
* CALL CNVTHP(A(I),A(J),MODE)
* CONVERTS HEWLETT-PACKARD 16-BIT NUMBERS IN WORDS A(I) THROUGH A(J) (J.GE.I)
* TO CDC FORMAT 60-BIT QUANTITIES.
* MODE=0 (OR OMITTED): FLOATING POINT      MODE=1: INTEGER
* FLOATING POINT CONVERSION MAKES 1 CDC TYPE 'REAL' WORD FROM 2 CONSECUTIVE
* HP 16-BIT WORDS.
* CORRECTED JANUARY 1982; J.STRACHOTA, EP /J.S./
 NAME     VFD         36/6HCNVTHP,24/3
          ENTFTN      CNVTHP,3,B7
          SA0         B1               A(I) SET UP INITIAL CONDITIONS
          SB2         B2-B1
          SB1         B0               LOOP INDEX
          SB2         B2+1             I-J+1
          SX2         B7-2             NO. ARGUMENTS-2
          ZR          X2,FLOAT         IF 2 ARGS. ASSUME FLOATING
          NG          X2,CNVTHP        LT 2 ARGS. RETURN
          SA1         B3               3 ARGS.
          ZR          X1,FLOAT
*
          MX0         1                INTEGERS
          LX0         16
 LOOPI    SA1         A0+B1
          BX2         X0*X1            EXTRACT SIGN BIT
          LX2         44
          BX6         X2
          LX2         1
          AX6         43               SIGN EXTENSION
          BX6         X6+X1
          IX6         X6-X2            SUBTRACT SIGN BIT
          SA6         A1
          SB1         B1+1
          LT          B1,B2,LOOPI
V         EQ          CNVTHP           RETURN
*
 FLOAT    SB3         B0               FLOATING POINT
 LOOP     SA1         A0+B1            A(I)
          SA4         A1+1             SECOND HALF OF FL. PT. NUMBER
          LX1         16
          BX1         X1+X4
          ZR          X1,L             IF ZERO GO TO END OF LOOP
          MX0         1
          LX0         32
          BX2         X0*X1            SIGN BIT OF FRACTIONAL PART, S1
          LX2         28
          BX6         X2
          AX6         24
          LX2         1                S1 RIGHT-ADJUSTED
          LX6         25
          MX0         23
          LX0         31               EXTRACT FRACTIONAL PART OF A(I)
          BX4         X0*X1
          AX4         8
          IX4         X4-X2
          LX4         25
          BX6         X6+X4            PUT FRACTIONAL PART IN NEW
          MX0         59
          BX4         -X0*X1
          BX3         X4               SIGN BIT OF EXPONENT, S2
          LX4         59
          AX4         52
          MX0         7
          LX0         8
          BX5         X0*X1            EXTRACT EXPONENT OF A(I)
          AX5         1
          BX4         X4+X5
          IX4         X4-X3
          SX7         60D             .J.S.
          IX7         X4-X7           .J.S.
          NG          X7,L005         .J.S.
          SX4         X4+1            .J.S.
 L005     BSS         0               .J.S.
          SX4         X4+1717B         ADD BIAS TO EXPON5NT
          ZR          X2,JUMP
          BX4         -X4              S1=1, COMPLEMENT
 JUMP     MX0         48
          BX5         -X0*X4
          LX5         48
          BX6         X6+X5            NEW COMPLETE
          NX6         X6
          SA6         A0+B3
 L        SB1         B1+2
          SB3         B3+1
          LT          B1,B2,LOOP
          EQ          CNVTHP
          END
