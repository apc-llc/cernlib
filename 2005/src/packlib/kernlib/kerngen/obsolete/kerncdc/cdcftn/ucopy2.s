*
* $Id: ucopy2.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: ucopy2.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT UCOPY2
*
* CERN PROGLIB# V301    UCOPY2          .VERSION KERNCDC  2.01  820727
*
*         MOVE BLOCKS OF STORAGE
*
*     SUBROUTINE UCOPY2 (S,T,N)
*
*         OVERLAP ALLOWED
*
          ENTRY UCOPY2
          VFD   36/6HUCOPY2,24/UCOPY2
 UCOPY2   BSS   1
*                              X1= ADR(S)
          SB7   1              B7= 1
          SA2   A1+B7          X2= ADR(T)
          SA3   A2+B7          X3= ADR(N)
          SB1   X1             B1= ADR(S)
          SB6   B7             B6= +1  IF NOT-INV
          SB5   B0                 START BIAS =0 IF NOT-INV
          SB1   B0-B1          B1= -ADR(S)
          SA5   X3             X5= N
          SB3   X2+B1          B3= DISTANCE = ADR(T) - ADR(S)
          EQ    B3,B0,UCOPY2       NO REAL COPY
          LT    B3,B0,NOTINV
          SB6   B0-B7          B6= -1  IF INV
          SB5   X5+B6              START BIAS =N-1 IF INV
*
 NOTINV   SA1   X1+B5          A1= ADR(S(FIRST))
          SB2   B7+B7          B2= 2
          SA2   A1+B6          A2= ADR(S(SECOND))
          SX0   B2+B2          X0= 4
          SB6   B6+B6          B6= +/- 2 FOR NOT/INV
          IX5   X5-X0          X5= N-4 FOR N WORDS STILL TO BE DONE
          NG    X5,ENDL            LT 4 WORDS
*
 LOOP     SA3   A1+B6              TRANSFER 4 WORDS AT A TIME
          BX6   X1
          SA4   A2+B6
          AX7   X2
          SA6   A1+B3
          SA7   A2+B3
          IX5   X5-X0
          SA1   A3+B6
          BX6   X3
          SA2   A4+B6
          AX7   X4
          SA6   A3+B3
          SA7   A4+B3
          PL    X5,LOOP
 ENDL     BX6   X1
          SB5   X5+B2          B5= N-2 FOR N WORDS STILL TO BE DONE
          SB4   B5+B2          B4= N
          LT    B4,B7,UCOPY2       EXIT  N < 1
          SA6   A1+B3
          BX7   X2
          EQ    B4,B7,UCOPY2       EXIT  N=1
          SA7   A2+B3
          SA1   A1+B6
          EQ    B5,B0,UCOPY2       EXIT  N=2
          BX6   X1
          SA6   A1+B3
          EQ    UCOPY2
          END
#ifdef CERNLIB_TCGEN_UCOPY2
#undef CERNLIB_TCGEN_UCOPY2
#endif
