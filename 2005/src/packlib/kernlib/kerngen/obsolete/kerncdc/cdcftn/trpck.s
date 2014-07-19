*
* $Id: trpck.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trpck.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRPCK
*
* CERN PROGLIB# F112    TRPCK           .VERSION KERNCDC  0.1   761001
*
          ENTRY TRPCK
*     SUBROUTINE TRPCK(A,S,M)
*     S:=A ,   A:  MXM,  S:  SYMETRIC MXM
*         WRITTEN BY ST.MEEN   14-JULY-76
          VFD   30/5HTRPCK,30/TRPCK
 TRPCK    BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA3   X3
          SB1   B0
          SB3   X3             B3= M
*
*                              NEWROW-LOOP: B1=1,...,M
 NEWROW   SB1   B1+B7
          SB5   X1
          SB2   B5+B1
*
*                              NEWCOL-LOOP: B5=X1,X1+1,...,X1+B1-1
*                              COPY S(X2)=A(B5)
 NEWCOL   SA5   B5
          BX6   X5
          SA6   X2
          SB5   B5+B7
          SX2   X2+B7
          LT    B5,B2,NEWCOL
*
          SX1   X1+B3          X1= X1+M
          LT    B1,B3,NEWROW
          JP    TRPCK
          END
#ifdef CERNLIB_TCGEN_TRPCK
#undef CERNLIB_TCGEN_TRPCK
#endif
