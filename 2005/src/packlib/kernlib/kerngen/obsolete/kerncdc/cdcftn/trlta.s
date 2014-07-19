*
* $Id: trlta.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trlta.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRLTA
*
* CERN PROGLIB# F112    TRLTA           .VERSION KERNCDC  0.1   761001
*
          ENTRY TRLTA
*     SUBROUTINE TRLTA  (U,A,B,M,N)
*     B=U.A,  U: LOWER TRIANGULAR MXM,  A: MXN,  B: MXN.
*         WRITTEN BY   ST. MEEN     22 JUNE-76
          VFD   30/5HTRLTA,30/TRLTA
 TRLTA    BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA4   A3+B7
          SA5   A4+B7
          SA1   X1
*
          SA4   X4
          SA5   X5             X5= N
          SX7   B7             X7= 1
          DX0   X5*X4
          IX0   X0+X5
          IX0   X7-X0          X0= 1-(M*N+N)
          BX1   X5             X1= N
          SB7   X4             B7= M
          SB3   B0
          SB4   B0
          SB6   X7             B6= 1
*
*         B1: INDEX OF U
*         B2: INDEX OF A
*         B3: INDEX OF B
*         B4:  U(B4) IS THE FIRST ELEMENT IN ROW B6
*         B5:  U(B1+B5) IS TO THE RIGHT OF U(B1)
*         B6:  COUNTS WHICH ROW IN U AND B
*         B7: M
*         X0: 1-(M*N+N)
*         X1: N
*         X7: 1
*
*                              NEWROW-LOOP: B6=1,2,...,M B4=0,2,5,9,......
 NEWROW   BSS   0
*                              NEWCOL-LOOP N TIMES  B1= B4,B1+B5,...
*                              B3 INCREASES EVERY RUN
 NEWCOL   SB1   B4
          SB5   B6
          SB2   B3
          MX6   0
*
*                              SUMUP-LOOP: B5=B6,B6+1,...,M
*                              MULTIPLIES COLUMN B6 OF U BY COL $=1,..,N
 SUMUP    SA4   A1+B1
          SA5   X2+B2
          RX5   X4*X5
          RX6   X6+X5
          NX6   B0,X6          X6= X6+U(B1)*A(B2)
*
          SB1   B1+B5          B1= B1+B5
          SB2   B2+X1          B2= B2+N
          SB5   B5+X7          B5= B5+1
          GE    B7,B5,SUMUP
*
          SA6   X3+B3              B(B3)=X6
          SB3   B3+X7          B3= B3+1
          SB2   B2+X0              IF B2 GT M*N+N-1 GO TO NEWCOL
          LT    B2,B0,NEWCOL
*
          SB6   B6+X7          B6= B6+1
          SB4   B4+B6
          GE    B7,B6,NEWROW
*
          JP    TRLTA
          END
#ifdef CERNLIB_TCGEN_TRLTA
#undef CERNLIB_TCGEN_TRLTA
#endif
