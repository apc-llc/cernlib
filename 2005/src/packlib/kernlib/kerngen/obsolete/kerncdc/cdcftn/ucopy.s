*
* $Id: ucopy.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: ucopy.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT UCOPY
*
* CERN PROGLIB# V301    UCOPY           .VERSION KERNCDC  2.01  820727
*
*         MOVE BLOCKS OF STORAGE
*
*     SUBROUTINE UCOPY (S,T,N)
*
*         OVERLAP NOT ALLOWED
*
          ENTRY UCOPY
          VFD   30/5HUCOPY,30/UCOPY
 UCOPY    BSS   1
*                              X1= ADR(S)
          SB7   1              B7= 1
          SA2   A1+B7          X2= ADR(T)
          SA3   A2+B7          X3= ADR(N)
          SB1   X1             B1= ADR(S)
          SB6   B7+B7          B6= 2
          SB1   B0-B1          B1= -ADR(S)
          SA5   X3             X5= N
          SB3   X2+B1          B3= DISTANCE = ADR(T) - ADR(S)
          SA1   X1             A1= ADR(S)
          SA2   A1+B7          A2= ADR(S+1)
          SX0   B6+B6          X0= 4
          EQ    B3,B0,UCOPY        NO REAL COPY
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
          SB5   X5+B6          B5= N-2 FOR N WORDS STILL TO BE DONE
          SB4   B5+B6          B4= N
          LT    B4,B7,UCOPY        EXIT  N < 1
          SA6   A1+B3
          BX7   X2
          EQ    B4,B7,UCOPY        EXIT  N=1
          SA7   A2+B3
          SA1   A1+B6
          EQ    B5,B0,UCOPY        EXIT  N=2
          BX6   X1
          SA6   A1+B3
          EQ    UCOPY
          END
#ifdef CERNLIB_TCGEN_UCOPY
#undef CERNLIB_TCGEN_UCOPY
#endif
