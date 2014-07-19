*
* $Id: trupck.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: trupck.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT TRUPCK
*
* CERN PROGLIB# F112    TRUPCK          .VERSION KERNCDC  0.1   761001
*
          ENTRY TRUPCK
*     SUBROUTINE TRUPCK(S,A,M)
*      A:=S,  S: SYMMETRIC MXM,  A: MXM
*         WRITTEN BY   ST.MEEN   16-JULY-76
          VFD   36/6HTRUPCK,24/TRUPCK
 TRUPCK   BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA3   X3
          SB3   X3             B3= M
*
*               COPY LOWER PART
          DX5   X3*X3
          SB6   X5
          SB6   B6+X2          B6= ADR(LAST ELEMENT IN A) +1=X2+M*M
*
          IX5   X5+X3
          AX5   1
          SB5   X5
          SB5   B5+X1          B5= ADR(LAST ELEMENT IN S) +1=X1+(M*M+M)/
*
          SB1   B3             B1= M
*                              NEWROW-LOOP: B1=M-1,...,0
*                              B6-X2=M*M,M*(M-1),...,M
 NEWROW   SB1   B1-B7
          SB2   B6-B1
*
*                              NEWCOL-LOOP: B6 ISSETDOWN BY 1 UNTIL B2-1
*                              B5 DECREASES BY 1 EVERY RUN
*                              COPY A(B6)=S(B5)
 NEWCOL   SB5   B5-B7
          SB6   B6-B7
          SA5   B5
          BX6   X5
          SA6   B6
          GE    B6,B2,NEWCOL
          SB6   B6-B3
          SB6   B6+B1          B6= B6-M+B1
          LT    B0,B1,NEWROW
*
*               COPY HIGHER PART
          SB1   B7
          SB4   X2
*                              NROW-LOOP: B1=2,3,...,M
 NROW     SB4   B4+B3
          SB5   B4
          SB6   X2+B1
          SB1   B1+B7
*                              NCOL-LOOP: B6=X2+B1,X2+B1+M,......
*                              B5=B4,B4+1,...,B4+B1-1
*                              COPY A(B6)=A(B5)
 NCOL     SA5   B5
          BX6   X5
          SA6   B6
          SB5   B5+B7
          SB6   B6+B3
          LT    B6,B5,NCOL
          LT    B1,B3,NROW
          JP    TRUPCK
          END
#ifdef CERNLIB_TCGEN_TRUPCK
#undef CERNLIB_TCGEN_TRUPCK
#endif
