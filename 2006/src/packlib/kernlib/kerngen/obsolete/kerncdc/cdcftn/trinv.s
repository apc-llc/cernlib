*
* $Id: trinv.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trinv.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRINV
*
* CERN PROGLIB# F112    TRINV           .VERSION KERNCDC  0.1   761001
*
          ENTRY TRINV
*     SUBROUTINE TRINV(S,S1,M)
*     S1= INV(S)       S,S1: LOWER TRIANGULAR MXM
*         WRITTEN BY   ST. MEEN   22-JULY-76
          VFD   30/5HTRINV,30/TRINV
 TRINV    BSS   1
          SB7   -1
          SA2   A1-B7
          SA3   A2-B7
          SA3   X3
*
          SB3   X3             B3= M
          SB1   -B7            B1= 1
          SB5   B0
          SB6   B0
          SA3   =1.0           X3= 1,0
*
*                              LOOP1: B1=1,2,...,M  B6=0,2,5,9,...
 LOOP1    SA5   X1+B6          X5= DIAG-ELEM NO B1 OF S
          MX6   0
          NG    X5,Z
          ZR    X5,Z               IF X5 .GT. 0 THEN X6=1/X5 ELSE X6=0
          RX6   X3/X5
 Z        SA6   X2+B6              S1(B6) =X6 (DIAG-ELEM)
          BX6   -X6            X6= -X6
          SB4   B0
*
*                              LOOP2: B4=-1,-2,...UNTIL B5=B6
*                              B5 INCREASES BY 1 EVERY RUN
          EQ    B5,B6,OUT
 LOOP2    MX7   0
          ZR    X6,ZERO
          SB2   -B1
          SB4   B4+B7
          SX4   X2+B5
          SX5   X1+B6
*
*                              "MULTIPLIES" ROW B1 OF S BY COL B4 OF S1
*                              EXEPT DIAG-ELEM, RESULT IN X7
*                              SUMUP-LOOP: B2=-B1+1,-B1+2,...,-B4
 SUMUP    SB2   B2-B7
          SA4   X4+B2
          SA5   X5+B7
          RX5   X5*X4
          RX7   X7+X5
          NX7   B0,X7
          SX4   A4
          SX5   A5
          LT    B2,B4,SUMUP
*
          RX7   X6*X7          X7= X6*X7=-X7/S(B6)
 ZERO     SA7   X2+B5              S1(B1)= X7
          SB5   B5-B7
          NE    B5,B6,LOOP2
*
 OUT      SB5   B5-B7          B5= B5+1
          SB1   B1-B7          B1= B1+1
          SB6   B6+B1
          GE    B3,B1,LOOP1
*
          JP    TRINV
          END
#ifdef CERNLIB_TCGEN_TRINV
#undef CERNLIB_TCGEN_TRINV
#endif
