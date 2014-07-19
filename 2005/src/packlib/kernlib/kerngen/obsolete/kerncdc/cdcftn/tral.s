*
* $Id: tral.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: tral.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT TRAL
*
* CERN PROGLIB# F112    TRAL            .VERSION KERNCDC  0.1   761001
*
          ENTRY TRAL
*     SUBROUTINE TRAL (A,U,B,M,N)
*         WRITTEN BY   ST. MEEN   24-JUNE-76
          VFD   24/4HTRAL,36/TRAL
 TRAL     BSS   1
          SB7   -1
          SA2   A1-B7
          SA3   A2-B7
          SA4   A3-B7
          SA5   A4-B7
*
          SX7   -B7            X7= 1
          SA4   X4
          SA5   X5
          SB6   X4             B6= M
          SB7   X5             B7= N
          SB3   B0
*
*                              NEWROW-LOOP: B6=M,M-1,...,1
NEWROW    MX0   0
          SB5   X7             B5= 1
*
*                              NEWCOL-LOOP: B5= 1,...,N X0=0,2,5,...
*                              B3 INCREASESE BY 1 EVERY RUN
NEWCOL    SB1   B3
          SB2   X0
          MX6   0
          SB4   B5
*
*                              SUMUP-LOOP: B4=B5,B5+1,...,N
*                              MULTIPLIES ROW B5 OF A BY COL M-B6 OF U
 SUMUP    SA4   X1+B1
          SA5   X2+B2
          RX5   X4*X5
          RX6   X6+X5
          NX6   B0,X6          X6= X6+A(B1)*U(B2)
*
          SB1   B1+X7
          SB2   B2+B4
          SB4   B4+X7          B4= B4+1
          GE    B7,B4,SUMUP        IF B4LE N GOTO SUMUP
*
          SA6   X3+B3              B(B3)=X6
          SB3   B3+X7
          SB5   B5+X7
          SX0   X0+B5
          GE    B7,B5,NEWCOL       IF B5 LE N GOTO NEWCOL
*
          SB6   B6-1
          NE    B6,B0,NEWROW       IF B6NE 0 GOTO NEWROW
          JP    TRAL
          END
#ifdef CERNLIB_TCGEN_TRAL
#undef CERNLIB_TCGEN_TRAL
#endif
