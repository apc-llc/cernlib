*
* $Id: flpsor.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: flpsor.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT FLPSOR
*
* CERN PROGLIB# M103    FLPSOR          .VERSION KERNCDC  1.05  780503
*
          ENTRY FLPSOR
*
*         THIS SUBROUTINE PACKAGES WAS ENTIRELY RECODED IN MARCH 74
*         TO OVERCOME A DESIGN BUG IN THE PREVIOUS PACKAGE, THAT,
*         DEPENDING ON THE SEQUENCE OF THE NUMBERS TO BE SORTED,
*         OVERWROTE A NUMBER BY ANOTHER ONE.
*         H. V. EICKEN, CERN, DD-DIVISION
*
*     PROGRAM  M103  TAKEN FROM CERN PROGRAM LIBRARY,  29-APR-78
*
* SUBROUTINE FLPSOR(A,N)
*
*         THIS SUBROUTINE SORTS FLOATING POINT NUMBERS
*         IT SORTS AN ARRAY INTO ITSELF
*         THE CALLING SEQUENCE IS * CALL FLPSOR ( ARRAY,NUMBER )
*
          USE   /SLATE/
          BSS   10
 STACK    BSS   30
          USE
*
          VFD   36/0HFLPSOR,24/FLPSOR
FLPSOR    EQ    *+1S17             EXIT ENTRY LINE
          SB1   1                  ALWAYS 1
          SA2   A1+B1
          SB7   B0-B1              STACK POINTER. EMPTY STACK
          SA3   X2                 LOAD NUMBER OF ELEMENTS
          SX4   X1+B7              START OF ARRAY - 1
          IX5   X4+X3              ADDRESS OF LAST ELEMENT
          LX1   20
          BX1   X1+X5              FIRST AND LAST ELEMENT
          SB3   X3+B7              NUMBER OF ELEMENTS -1
          GE    B0,B3,FLPSOR       LESS THAN TWO ELEMENTS
SPLIT     SB4   X1                 UPPER BOUNDARY
          AX1   20
          SB2   X1                 LOWER BOUNDARY
          SX0   B2+B4
          SA2   B2                 LOAD LOWER ELEMENT
          SA4   B4                 LOAD UPPER ELEMENT
          AX0   1                  ADDRESS OF CENTER ELEMENT
          FX5   X4-X2
          SB3   B3-B1              REDUCE NUMBER OF ELEMENTS
          BX6   X2                 X6 CONTAINS SMALLEST ELEMENT
          SA3   X0                 LOAD EVENTUAL CENTER ELEMENT
          LX7   X4                 X7 CONTAINS LARGEST ELEMENT
          PL    X5,L1              IF UPPER .GT. LOWER
          BX6   X4                 SWAP THEM
          LX7   X2,B0
L1        FX5   X7-X3
          ZR    B3,L4              MORE THAN TWO ELEMENTS?
          BX4   X7                 ALLOW FOR A SWAP
          PL    X5,L2              IF UPPER .GT. CENTER
          BX7   X3                 SWAP THEM
          LX3   X4,B0
L2        FX5   X3-X6              MIGHT BE UNNECESSARY
          BX4   X6                 X4 CONTAINS ASSUMED LOWEST
          LX6   X3,B0              X6 CONTAINS ASSUMED CENTER
          SB3   B3-B1              REDUCE NUMBER AGIN
          PL    X5,L3              IF CENTER .GT. LOWER
          BX6   X4                 SWAP THEM
          LX4   X3
L3        SA6   A3                 STORE CENTER
          SB5   B4                 SET RUNNING POINTER IN UPPER
          LX3   X6                 KEEP CENTER VALUE IN X3
          BX6   X4
L4        SA6   A2                 STORE LOWER
          SA7   A4                 STORE UPPER
          ZR    B3,NEXT            INTERVAL IS SORTED
          SB3   B2                 RUNNING POINTER IN LOWER
UPPER     SB5   B5-B1              REDUCE POINTER
          SA4   B5                 LOAD ELEMENT
          FX0   X4-X3              COMPARE AGINST SPLITTING ELEMENT
          BX6   X4                 PREPARE A SWAP
          EQ    B3,B5,SPLITED      INTERVAL IS SPLITTED
          PL    X0,UPPER
LOWER     SB3   B3+B1              INCREMENT POINTER
          SA2   B3                 LOAD ELEMENT
          FX0   X3-X2              COMPARE AGAINST SPLITTING ELEMENT
          BX7   X2                 PREPARE A SWAP
          EQ    B3,B5,SPLITED
          PL    X0,LOWER
          SA6   A2                 SWAP ELEMENTS
          SA7   A4
          EQ    UPPER
SPLITED   SX1   B2                 LOWER BOUNDARY IN LOWER INTERVAL
          SX2   B3                 UPPER BOUNDARY IN LOWER INTERVAL
          SB5   B5+B1
          LX1   20
          SX3   B5                 LOWER BOUNDARY IN UPPER INTERVAL
          SX4   B4                 UPPER BOUNDARY IN UPPER INTERVAL
          BX1   X1+X2              ASSUME LOWER IS SMALLER
          LX3   20
          BX6   X3+X4              ASSUME UPPER IS LARGER
          SB3   B3-B2              # OF LEMENTS - 1 IN LOWER
          SB4   B4-B5              # OF ELEMNTS - 1 IN UPPER
          LX5   X6,B0              PREPARE A SWAP
          SX0   B4
          GE    B4,B3,L5
          BX6   X1                 SWAP THEM
          LX1   X5,B0
          SB2   B3
          SB3   X0
          SX0   B2
L5        NZ    B3,STOR            MORE THAN ONE ELEMENT IN SMALLER?
          BX1   X6                 NO, SMALLER IS SORTED
          SB3   X0
          EQ    SPLIT
STOR      LX6   20
          BX7   X6+X0
          SB7   B7+B1              INCREMENT STACK POINTER
          SA7   STACK+B7           STORE INTERVAL
          EQ    SPLIT
NEXT      SA1   STACK+B7           LOAD NEXT INTERVAL
          NG    B7,FLPSOR          STACK IS EMPTY, ALL DONE
          SB7   B7-B1              DECREMENT STACK POINTER
          SB3   X1                 NUMBER OF ELEMENTS - 1
          AX1   20
          EQ    SPLIT
          END
#ifdef CERNLIB_TCGEN_FLPSOR
#undef CERNLIB_TCGEN_FLPSOR
#endif
