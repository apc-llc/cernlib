*
* $Id: uctoh.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: uctoh.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT UCTOH
*
* CERN PROGLIB# M409    UCTOH           .VERSION KERNCDC  2.07  831121
* ORIG. 14/11/83  JZ, CERN
*
*     SUBROUTINE UCTOH (CH,HOLL,K,NCH)
*
*-----             USAGE OF REGISTERS
*
*         B1    NO. OF CHARS STILL TO BE TAKEN FROM CUR. INPUT CH(J)
*         B2    = 10   CHAR PER INPUT WORD
*         B3    = 6*(10-K)  SHIFT RESULT WORD
*         B4    = NCH COUNTED DOWN TO ZERO
*         B5    = K = CHAR CAPACITY OF HOLL
*         B6    NO. OF CHARS STILL TO BE PLACED INTO CUR. OUTPUT HOLL
*         B7    = -1
*
*      A1,X1    INPUT WORD CH(J)
*         X0    MASK 77B FOR RIGHT-MOST CHARACTER
*         X4    MASK 00..0055..55 OF (10-K) BLANKS
*         X5    = 6  FOR INTEGER MULTIPLY
*      A6,X6    OUTPUT WORD HOLL(L)
*
          ENTRY UCTOH
          VFD   30/5HUCTOH,30/UCTOH
 UCTOH    BSS   1
          SB7   -1            *B7= -1
*                              X1= ADR(CH)
          SA2   A1-B7          X2= ADR(HOLL)
          SA3   A2-B7          X3= ADR(K)
          SA4   A3-B7          X4= ADR(NCH)
          MX0   6
          LX0   6             *X0= 77B  MASK OF 1 CHAR.
*
          SB6   X2             B6= ADR(HOLL)
          SA3   X3             X3= K
          SA4   X4             X4= NCH
*
          SA2   B6+B7          A2= ADR(HOLL) - 1
          SB2   10            *B2= 10
          ZR    X4,UCTOH           EXIT IF NCH=0
*
          BX6   X2
          SA6   A2            *A6= ADR(HOLL) - 1
          SB4   X4            *B4= NCH
          SX5   6             *X5= 6
*
*--             CALC. B5 = K = MIN0 (K,10)
*
          SX6   B2             X6= 10
          IX7   X6-X3          X7= 10 - K
          AX7   59             X7= MASK ALL ZERO OR ALL ONE
          BX6    X7*X6         X6= ZERO OR 10
          BX3   -X7*X3         X3=    K OR ZERO
          BX3   X3+X6          X3= MIN0 (K,10)
          SB5   X3            *B5= K
*
*--             MAKE MASK OF 10-K BLANKS BY SHIFTING 6*(K-1) BITS
*               B3 IS SHIFT-COUNT OF OUTPUT WORDS 6*(10-K) BITS
*
          SA4   BLANK          X4= ZERO/9 BLANKS
          SX2   B5+B7          X2=  K - 1
          SX3   B2-B5          X3= 10 - K
          DX2   X2*X5          X2= 6*(K-1)
          DX3   X3*X5          X3= 6*(10-K)
          SB6   X2             B6= 6*(K-1)
          SB3   X3            *B3= 6*(10-K)
          AX4   B6,X4         *X4=   MASK WITH 10-K BLANKS
*
*--             READY FIRST INPUT WORD
*
#if !defined(CERNLIB_F77)
          SA1   X1            *A1= ADR(CH)
          SB1   B2            *B1= 10      TAKE 10 CHARS
#endif
#if defined(CERNLIB_F77)
          SB6   X1             B6= ADR(CH)
          AX1   24
          BX2   X1*X0          X2= NSK = SKIP NSK FIRST CHARS
          SA1   B6            *A1= ADR(CH)
          SB1   X2             B1= NSK
          DX2   X2*X5          X2= 6*NSK
          SB6   X2             B6= 6*NSK
          LX1   B6,X1         *X1= CH(1)  ROTATED READY
          SB1   B2-B1         *B1= 10-NSK CHARS STILL TO BE TAKEN
#endif
*
*--             START LOOPING
*
          SA2   A1-B7         *X2= LOOK-AHEAD INPUT WORD
          MX6   0             *X6= ZERO, OUTPUT WORD CLEAR
          SB6   B5            *B6= K CHARS PER OUTPUT WORD
*
*-----          COPY NEXT CHARACTER
*
 LOOPCH   LX1   6
          SB1   B1+B7          B1  CHARS TO BE TAKEN FROM CURRENT
          LX6   6
          SB4   B4+B7          B4  CHARS TO BE DONE IN ALL
          BX7   X0*X1          X7= 1 INPUT CHAR ISOLATED
          SB6   B6+B7          B6  CHARS TO BE PUT INTO CURRENT
          BX6   X6+X7                OUTPUT CHARS ACCUMULATED
          EQ    B4,B0,DONE
          EQ    B6,B0,NEXTOUT
          NE    B1,B0,LOOPCH
*
*--             LOAD NEXT INPUT WORD
*
 NEXTIN   BX1   X2
          SA2   A2-B7          X2  NEXT LOOK-AHEAD INPUT WORD
          SB1   B2             B1= 10
          JP    LOOPCH
*
*--             STORE CURRENT OUTPUT WORD, READY FOR NEXT
*
 NEXTOUT  LX6   B3,X6
          BX6   X4+X6          X6  OUTPUT WORD WITH BLANK-FILL
          SA6   A6-B7              STORED
          SB6   B5             B6= K
          MX6   0
          NE    B1,B0,LOOPCH       CURRENT INPUT NOT EXHAUSTED
          JP    NEXTIN
*
*-----          ALL DONE, STORE LAST OUTPUT WORD
*
*                           NKEEP = 10-NDONE  TRAILING CHARS
*                         - LEFT  SHIFT RESULT BY 6*NKEEP
*                         - MERGE NKEEP TRAILING BLANKS
*
 DONE     SB6   B5-B6          B6= NDONE
          SB6   B2-B6          B6= NKEEP = 10-NDONE
          EQ    B6,B0,STORE
          SX2   B6             X2= NKEEP
          MX3   1
          DX2   X2*X5          X2= 6*NKEEP
          SA1   BLANK          X1= ZERO WITH 9 TRAILING BLANKS
          SB1   X2             B1= 6*NKEEP
          SB2   X2+B7          B2= 6*NKEEP - 1
          LX6   B1,X6          X6= NDONE CHARS / ZERO
          AX3   B2,X3          X3= MASK OF 6*NKEEP BITS LEFT
          LX3   B1,X3          X3= MASK OF 6*NKEEP BITS RIGHT
          BX1   X3*X1          X1= NKEEP BLANKS ISOLATED
          BX6   X1+X6
STORE     SA6   A6-B7              STORE
          JP    UCTOH
 BLANK    DATA  00555555555555555555B
          END
#ifdef CERNLIB_TCGEN_UCTOH
#undef CERNLIB_TCGEN_UCTOH
#endif
