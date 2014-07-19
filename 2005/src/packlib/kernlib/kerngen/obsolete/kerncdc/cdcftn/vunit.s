*
* $Id: vunit.s,v 1.1.1.1 1996/02/15 17:51:28 mclareni Exp $
*
* $Log: vunit.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:28  mclareni
* Kernlib
*
*
          IDENT VUNIT
*
* CERN PROGLIB# F121    VUNIT           .VERSION KERNCDC  0.1   750820
*
*        CALL VUNIT(A,X,N)
*        SETS
*        X(I) = A(I) / MODULUS(A)   FOR   I = 1,N
*
*
          ENTRY VUNIT
          EXT   SQRT
          VFD   30/5HVUNIT,30/VUNIT
 VUNIT    DATA  0
          SB7   -1
          SA2   A1-B7
          SB1   X1
          BX6   X2
          SA4   A2-B7
          SA6   TEMP2              STORE B
          SA3   X4                 PICK UP N
          MX6   0                  X6 IS TO HOLD SUM OF SQUARES
          SB5   X3                 B5 = N
          BX7   X3
          LE    B5,B0,VUNIT        RETURN IF N LE ZERO
          SB4   B0                 B4 IS TO HOLD I-1
          SA7   TEMP3              STORE N
 LOOP1    SA1   B1+B4              PICK UP NEXT A(I)
          RX4   X1*X1              X4 = A(I)**2
          SB4   B4+1               INCREMENT B4 (I)
          RX7   X6+X4              ADD ON A(I)**2
          NX6   B0,X7              NORMALISE
          LT    B4,B5,LOOP1        LOOP
          SA6   TEMP4              STORE SUM OF SQUARES
          SX7   B1
          SX6   TEMP4
          SA7   TEMP1              STORE A
          SA6   ARG
          MX7   0
          SA7   A6-B7
          SA1   ARG
 +        RJ    SQRT
 -        LT    B0,B1,VUNIT-1
          SA5   =1.0               X5 = 1.0
          RX4   X5/X6              FORM INVERSE OF X6
          SA3   TEMP3              PICK UP N
          SA1   TEMP1              PICK UP A
          SA2   TEMP2              PICK UP B
          SB5   X3-1               B5 = N-1
          SA3   X1                 PICK UP A(1)
          ZR    X6,ZERO            JUMP IF SUM OF SQUARES IS ZERO
          SB4   B0                 B4 IS TO HOLD I-1
          RX6   X3*X4              MULTIPLY IT BY REQUIRED FACTOR
          EQ    B5,B0,FIN
 LOOP2    SA6   X2+B4              STORE RESULT IN B(I)
          SB4   B4+1               INCREMENT I
          NO
          SA3   X1+B4              PICK UP NEXT A(I)
          RX6   X3*X4              MULTIPLY IT BY REQUIRED FACTOR
          LT    B4,B5,LOOP2        LOOP
 FIN      SA6   X2+B4              STORE B(N)
          EQ    B0,B0,VUNIT        RETURN
 ZERO     SX6   B0                 RETURN A ZERO VECTOR
 LOOP3    SA6   X2+B5
          SB5   B5-1
          GE    B5,B0,LOOP3
          EQ    B0,B0,VUNIT
 TEMP1    BSS   1
 TEMP2    BSS   1
 TEMP3    BSS   1
 TEMP4    BSS   1
 ARG      BSS   2
          END
#ifdef CERNLIB_TCGEN_VUNIT
#undef CERNLIB_TCGEN_VUNIT
#endif
