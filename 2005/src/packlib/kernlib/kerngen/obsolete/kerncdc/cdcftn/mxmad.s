*
* $Id: mxmad.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: mxmad.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT MXMAD              10-JUNE-68
*
* CERN PROGLIB# F110    MXMAD           .VERSION KERNCDC  0.1   750820
*
          ENTRY MXMAD
          ENTRY MXMAD1
          ENTRY MXMAD2
          ENTRY MXMAD3
          ENTRY MXMPY
          ENTRY MXMPY1
          ENTRY MXMPY2
          ENTRY MXMPY3
          ENTRY MXMUB
          ENTRY MXMUB1
          ENTRY MXMUB2
          ENTRY MXMUB3
*     SUBROUTINE MXMAD  (A,B,C,I,J,K)
          VFD   30/5HMXMAD,30/MXMAD
 MXMAD    BSS   1
          SA3   MXMAD
          SX2   0
          SA4   XMAD
          EQ    LOC1
*     SUBROUTINE MXMAD1 (A,B,C,I,J,K)
          VFD   36/6HMXMAD1,24/MXMAD1
 MXMAD1   BSS   1
          SA3   MXMAD1
          SX2   1
          SA4   XMAD
          EQ    LOC1
*     SUBROUTINE MXMAD2 (A,B,C,I,J,K)
          VFD   36/6HMXMAD2,24/MXMAD2
 MXMAD2   BSS   1
          SA3   MXMAD2
          SX2   2
          SA4   XMAD
          EQ    LOC1
*     SUBROUTINE MXMAD3 (A,B,C,I,J,K)
          VFD   36/6HMXMAD3,24/MXMAD3
 MXMAD3   BSS   1
          SA3   MXMAD3
          SX2   3
          SA4   XMAD
          EQ    LOC1
*     SUBROUTINE MXMPY  (A,B,C,I,J,K)
          VFD   30/5HMXMPY,30/MXMPY
 MXMPY    BSS   1
          SA3   MXMPY
          SX2   0
          SA4   XMPY
          EQ    LOC1
*     SUBROUTINE MXMPY1 (A,B,C,I,J,K)
          VFD   36/6HMXMPY1,24/MXMPY1
 MXMPY1   BSS   1
          SA3   MXMPY1
          SX2   1
          SA4   XMPY
          EQ    LOC1
*     SUBROUTINE MXMPY2 (A,B,C,I,J,K)
          VFD   36/6HMXMPY2,24/MXMPY2
 MXMPY2   BSS   1
          SA3   MXMPY2
          SX2   2
          SA4   XMPY
          EQ    LOC1
*     SUBROUTINE MXMPY3 (A,B,C,I,J,K)
          VFD   36/6HMXMPY3,24/MXMPY3
 MXMPY3   BSS   1
          SA3   MXMPY3
          SX2   3
          SA4   XMPY
          EQ    LOC1
*     SUBROUTINE MXMUB  (A,B,C,I,J,K)
          VFD   30/5HMXMUB,30/MXMUB
 MXMUB    BSS   1
          SA3   MXMUB
          SX2   0
          SA4   XMUB
          EQ    LOC1
*     SUBROUTINE MXMUB1 (A,B,C,I,J,K)
          VFD   36/6HMXMUB1,24/MXMUB1
 MXMUB1   BSS   1
          SA3   MXMUB1
          SX2   1
          SA4   XMUB
          EQ    LOC1
*     SUBROUTINE MXMUB2 (A,B,C,I,J,K)
          VFD   36/6HMXMUB2,24/MXMUB2
 MXMUB2   BSS   1
          SA3   MXMUB2
          SX2   2
          SA4   XMUB
          EQ    LOC1
*     SUBROUTINE MXMUB3 (A,B,C,I,J,K)
          VFD   36/6HMXMUB3,24/MXMUB3
 MXMUB3   BSS   1
          SA3   MXMUB3
          SX2   3
          SA4   XMUB
*
 LOC1     SB7   1
          SA5   A1+B7
          BX6   X3
          LX7   B0,X4
          SA6   MXMAD              STORE E/E LINE
          SA4   A5+B7
          SA7   LOOP23             STORE APPROPRIATE INSTRUCTIONS
          SB2   X5
          SA5   A4+B7
          SB3   X4
          MX7   59
          SA4   X5             X4= I
          SB1   X1
          ZR    X4,MXMAD
          SA1   A5+B7
          BX6   -X4            X6= -I
          SA3   X1             X3= J
                               X2= N2=0, 1, 2, 3
          BX7   X2*X7
          SA4   A1+B7
          SB6   X3             B6= J
          SA1   B1             A1= B1
          SA5   X4             X5= K
          SX0   1              X0= 1
          SA6   MOINSI
          ZR    X5,MXMAD
          BX4   -X6
          SX6   A0
          SA6   SAVEA0
          NZ    X7,LOC5
          SX6   X3                 IOA=J
          BX4   X0                 IIA=1
          EQ    LOC7
 LOC5     SX6   X0                 IOA=1,  IIA=I
 LOC7     BX7   X0*X2
          LX6   20
          NZ    X7,LOC11
          SB1   X5
          IX6   X6+X0              IOB=1,  IIB=K
          EQ    LOC13
 LOC11    SB1   X5
          BX5   X0                 IIB=1
          IX6   X6+X3              IOB=J
 LOC13    SA0   B2             A0= ADDRESS OF B(1)
          SB5   B0
          NO
          SB2   X0             B2= 1
 LOOP25   SB4   A0
          SB3   B3+B5
          NO
          SB5   B0             B5= M
 LOOP23   BSS   1
          SA2   A1
          SB7   B0             B7= N
          NO
          SA3   B4
          SX1   B4             X1= JB
          NO
          NO
          SX0   A2             X0= JA
 LOOP20   RX2   X2*X3
          FX1   X1+X5          X5= IIB
          SB7   B2+B7
          NX7   B0,X7
          IX0   X0+X4          X4= IIA
          SA3   X1             X3= B(JB)
          RX7   X7+X2
          SA2   X0             X2= A(JA)
          NE    B6,B7,LOOP20
          NX7   B0,X7
          SB4   X6+B4          X6= IOA - IOB PACKED
 LOC23    SA7   B3+B5
          SB5   B2+B5
          NE    B5,B1,LOOP23   B1= K
          LX6   40
          SA2   MOINSI
          SB7   X6
          SX7   X2+B2
          SA1   A1+B7
          LX6   20
          BX2   X7
          SA7   MOINSI
          NZ    X2,LOOP25
          SA1   SAVEA0
          SA0   X1
          EQ    MXMAD
 XMAD     SA3   B3+B5
          BX7   X3
          EQ    B6,B0,LOC23
 XMUB     SA3   B3+B5
          BX7   -X3
          EQ    B6,B0,LOC23
 XMPY     MX7   0
          EQ    B6,B0,LOC23
 MOINSI   BSS   1
 SAVEA0   BSS   1
          END
#ifdef CERNLIB_TCGEN_MXMAD
#undef CERNLIB_TCGEN_MXMAD
#endif
