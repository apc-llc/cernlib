*
* $Id: trla.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trla.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRLA
*
* CERN PROGLIB# F112    TRLA            .VERSION KERNCDC  0.1   761001
*
          ENTRY TRLA
*     SUBROUTINE TRLA (U,A,B,M,N)
*         WRITTEN BY   ST. MEEN   21-JUNE-76
          VFD   24/4HTRLA,36/TRLA
 TRLA     BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA4   A3+B7
          SA5   A4+B7
*
          SA4   X4             X4= M
          SA5   X5
          SB5   X5             B5= N
          DX5   X4*X5
          SB3   X5-1           B3= M*N -1
*
          DX5   X4*X4
          IX5   X5+X4
          AX5   1
          SB4   X5-1           B4= M*(M+1)/2-1
          SB6   -B5            B6= -N
*
*                               NEWROW-LOOP: WHILE B4 GE 0 (M TIMES)
 NEWROW   BSS   0
*                              NEWCOL-LOOP WHILE B2 GT -N (N TIMES
 NEWCOL   SB2   B3
          SB1   B4
          MX7   0
*
*                               SUMUP-LOOP: B2=B3,B3-N,... WHILE B2 GT 0
*                               MULTIPLIES ROW B3/M OF U BY COL B3 MOD N
*                               RESULT IN B(B3)
 SUMUP    SA4   X1+B1
          SA5   X2+B2
          RX5   X4*X5
          RX7   X7+X5
          NX7   B0,X7          X7= X7+U(B1)*A(B2)
*
          SB1   B1-B7          B1= B1-1
          SB2   B2-B5          B2= B2-N
          GE    B2,B0,SUMUP
*
          SA7   X3+B3              B(B3)=X7
          SB3   B3-B7          B3= B3-1
          LT    B6,B2,NEWCOL
*
          SB4   B1
          GE    B4,B0,NEWROW
          JP    TRLA
          END
#ifdef CERNLIB_TCGEN_TRLA
#undef CERNLIB_TCGEN_TRLA
#endif
