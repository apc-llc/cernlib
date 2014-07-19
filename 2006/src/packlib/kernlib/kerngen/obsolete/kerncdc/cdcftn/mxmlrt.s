*
* $Id: mxmlrt.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: mxmlrt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT MXMLRT
*
* CERN PROGLIB# F110    MXMLRT          .VERSION KERNCDC  0.1   760424
*
          ENTRY MXMLRT
          ENTRY MXMLTR
*
*     SUBROUTINE MXMLRT (A,B,C,NI,NJ)
          VFD   36/6HMXMLRT,24/MXMLRT
 MXMLRT   BSS   1
*
          SB7   1
          MX0   1
          SB1   X1
          SA2   A1+B7
          LX0   21             X0= X-IPA-X
          SB2   X2
          SA1   A2+B7
          SB3   X1
          SA2   A1+B7
          SA4   X2             X4= NI
          SA1   A2+B7
          SB4   X4             B4= NI
          SA5   X1             X5= NJ
          BX2   X5             X2= JPA=NJ
          EQ    LOC5
*
*     SUBROUTINE MXMLTR (A,B,C,NI,NJ)
          VFD   36/6HMXMLTR,24/MXMLTR
 MXMLTR   BSS   1
*
          SB7   1
          SA4   MXMLTR
          SB1   X1
          SA2   A1+B7
          BX6   X4
          SB2   X2
          SA1   A2+B7
          SX2   B7             X2= JPA=1
          SA6   MXMLRT
          SB3   X1
          SA5   A1+B7
          SA4   X5             X4= NI
          LX0   X4             X0= IPA=NI
          SA1   A5+B7
          LX0   20             X0= X-IPA-X
          SB4   X4             B4= NI
          SA5   X1             X5= NJ
*
 LOC5     IX0   X0+X2          X0= X-IPA-JPA
          MX3   59             X3= -1
          SB5   X5             B5= NJ
          LX0   20             X0= IPA-JPA-X
          SX5   B2             X5= B2
          EQ    B4,B0,MXMLRT
          SX1   B1             X1= B1
          EQ    B5,B0,MXMLRT
          LX5   20             X5= B2-X
          IX0   X0+X4          X0= IPA-JPA-II
          FX7   X1+X5          X7= B2-B1
          SA7   KPB2B1
          SX6   A0
          SA6   SAVEA0
          SA0   B1             A0= IA1
          MX6   0                  X=0.
*
 LOOP50   SB7   B3                 IC=IC1
          BX3   -X3            X3= 1
          LX0   20             X0= JPA-II-IPA
          SB6   B3+B4
 LOOP10   SA6   B7
          SB7   B7+X3
          LT    B7,B6,LOOP10
          SX4   B5             X4= NJ (FOR LOOP40)
          SB7   -1             B7= -1
 LOOP40   SA1   A0                 IA=IA1
          MX6   0                  X=0.
          SA2   B2                 IB=IB1
          NO
          SB6   X0             B6= IPA
          SX3   B5             X3= NJ (FOR LOOP20)
          LX0   20             X0= II-IPA-JPA
 LOOP20   SX3   X3+B7
          FX7   X1*X2
          SA1   A1+B6              IA=IA+IPA
          SA2   A2+B5              IB=IB+NJ
          FX5   X6+X7
          NX6   B0,X5
          NZ    X3,LOOP20
          SA1   B1                 JA=JA1
          SB6   X0             B6= JPA
          SA2   B3                 IC=IC1
          SX3   B4             X3= NI (FOR LOOP30)
 LOOP30   FX5   X6*X1
          SA1   A1+B6              JA=JA+JPA
          SX3   X3+B7
          FX7   X5+X2
          SA2   A2-B7              IC=IC+1
          NX7   B0,X7
          NO
          SA7   A2+B7
          NZ    X3,LOOP30
          SB2   B2-B7              IB1=IB1+1
          SX4   X4+B7
          LX0   40             X0= JPA-II-IPA
          SB1   B1+X0              JA1=JA1+IPA
          NZ    X4,LOOP40
          SA1   KPB2B1         X1= X-B2-B1
          LX0   20             X0= II-IPA-JPA
          SB3   B3+B4              IC1=IC1+NI
          SB6   X0             B6= JPA
          LX0   20             X0= IPA-JPA-II
          SB1   X1                 JA1=1
          SA0   A0+B6              IA1=IA1+JPA
          SX3   B7             X3= -1
          IX0   X0+X3
          SB6   X0
          LX1   40             X1= B1-X-B2
          SB2   X1                 IB1=1
          MX6   0
          NE    B6,B0,LOOP50
          SA1   SAVEA0
          SA0   X1
          EQ    MXMLRT
 KPB2B1   BSS   1
 SAVEA0   BSS   1
          END
#ifdef CERNLIB_TCGEN_MXMLRT
#undef CERNLIB_TCGEN_MXMLRT
#endif
