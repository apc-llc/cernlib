*
* $Id: mxequ.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: mxequ.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT MXEQU              10-JUNE-68
*
* CERN PROGLIB# F109    MXEQU           .VERSION KERNCDC  0.1   750820
*
          ENTRY MXEQU
          ENTRY MXEQU1
*
*     SUBROUTINE MXEQU (A,B,I,J)
          VFD   30/5HMXEQU,30/MXEQU
 SAVEA0   BSS   1
 MXEQU    BSS   1
          SB2   1              B2= 1
          SA2   =-1.           X2= -1.
          SB1   X1
          SA4   A1+B2
          SB7   B0             B7= L
          SA5   A4+B2
          SA1   B1             X1= A(1)
          SA3   X5             X3= I
          ZR    X3,MXEQU
          SX6   A0
          SB3   X3
          SA6   SAVEA0
          SA0   X4             A0= ADDRESS OF B
          SA4   A5+B2
          SA5   X4             X5= J
          BX7   X5
          MX4   0              X4= LL
          SA7   JLOCAL
 LOOP17   RX0   X2/X1          X0= DIAG
          SA5   A1+B2          X5= A(L1L)
          IX6   X4+X3
          SX1   B2             X1= 1
          SB6   B0             B6= M
          SB4   B7             B4= LM=L
          IX4   X6+X1              LL=LL+I+1
 LOOP12   SA1   B4+B1          X1= A(LM)
          EQ    B6,B7,DOWN12
          RX7   X1*X0
          NO
          NO
          NO
          SA7   A1
 DOWN12   SB6   B2+B6
          SB4   B4+B3              LM=LM+I
          LT    B6,B3,LOOP12
          SB5   B7+B2          B5= L1
          NO
          GE    B5,B3,BTRANS
 LOOP16   SX2   B7             X2= LM=L
          SB6   B0             B6= M
          NO
          SB4   B5-B7          B4= L1M-LM
 LOOP15   SA1   X2+B1          X1= A(LM)
          EQ    B6,B7,DOWN15
          RX6   X5*X1
          SA1   A1+B4          X1= A(L1M)
          RX1   X6+X1
          NX7   B0,X1
          SA7   A1
 DOWN15   SB6   B2+B6
          IX2   X2+X3              LM=LM+I
          LT    B6,B3,LOOP15
          SB5   B5+B2
          SA5   A5+B2
          LT    B5,B3,LOOP16
          SB7   B7+B2
          SA1   B1+X4          X1= A(LL)
          SA2   =-1.
          LT    B7,B3,LOOP17
 BTRANS   SA2   JLOCAL         X2= J
          EQ    STARTB
 JLOCAL   BSS   1
*
*     SUBROUTINE MXEQU1 (A,B,I,J)
          VFD   36/6HMXEQU1,24/MXEQU1
 MXEQU1   BSS   1
          SB2   1
          SA4   A1+B2
          SB1   X1
          SA1   MXEQU1
          BX6   X1
          SA2   A4+B2
          SA6   MXEQU
          SA3   X2             X3= I
          SA5   A2+B2
          ZR    X3,MXEQU
          SX7   A0
          SA2   X5             X2= J
          SA0   X4             A0= ADDRESS OF B
          SA7   SAVEA0
          SB3   X3             B3= I
 STARTB   ZR    X2,MXEQUE
          SA5   A0             X5= B(ML)
          SB4   B0
 LOOP27   BX0   -X2            X0= -J
          SB7   B4             B7= L
          SB4   B4+B2
 LOOP26   SX4   B7                 LN=L
          IX1   X0+X2
          SB5   B0             B5= N
          SB6   X1                 MN=M
 LOOP25   SA1   X4+B1          X1= A(LN)
          EQ    B5,B7,DOWN25
          RX6   X5*X1
          SA1   A0+B6          X1= B(MN)
          RX1   X6+X1
          NX7   B0,X1
          SA7   A1
 DOWN25   SB5   B5+B2
          IX4   X4+X3              LN=LN+I
          NO
          SB6   B6+X2              MN=MN+J
          LT    B5,B3,LOOP25
          SX0   X0+B2
          SA5   A5+B2
          NG    X0,LOOP26
          LT    B4,B3,LOOP27
          SB7   X3+B2          B7= I+1
          SA5   =1.            X5= 1.
          SA1   B1             X1= A(1)
          SB1   B0             B1= N
          SA4   A0             X4= B(1)
          NO
          SX3   B2             X3= 1
 LOOP36   RX6   X5/X1
          BX0   -X2            X0= -J
          SB1   B1+B2
          SA1   A1+B7          X1= A(NN)
 LOOP35   RX7   X6*X4
          IX0   X0+X3
          SA4   A4+B2          X4= B(MN)
          SA7   A4-B2
          NG    X0,LOOP35
          LT    B1,B3,LOOP36
 MXEQUE   SA1   SAVEA0
          SA0   X1
          EQ    MXEQU
          END
#ifdef CERNLIB_TCGEN_MXEQU
#undef CERNLIB_TCGEN_MXEQU
#endif
