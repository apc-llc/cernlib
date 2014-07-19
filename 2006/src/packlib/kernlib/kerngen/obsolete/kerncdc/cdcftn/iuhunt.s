*
* $Id: iuhunt.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: iuhunt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT IUHUNT
*
* CERN PROGLIB# V304    IUHUNT          .VERSION KERNCDC  0.1   760424
*
          ENTRY IUHUNT
*                              RE-WRITE FOR BUG-FIXING, JZ
*
*     J = IUHUNT (IT,IVEC,NV,INC)
*
          VFD   36/6HIUHUNT,24/IUHUNT
 FOUND    SX6   B6+B7          X6= J = JL+1
 IUHUNT   DATA  0
          SB7   1              B7= 1
          SA2   A1+B7
          SA3   A2+B7
          SA4   A3+B7
          SA1   X1             X1= IT
          SA2   X2             A2= ADR(IVECT(J))
          SA3   X3             X3= NV
          SA4   X4             X4= INC
          MX6   0              X6= J = ZERO
          SB6   X6             B6= (JL=J-1) = 0
          ZR    X3,IUHUNT          EXIT IF NV=0
          ZR    X4,INCZER          SPECIAL IF INC=0
          SB3   X3             B3= NV
          SB4   X4             B4= INC
 LOOP     BX0   X2-X1              COMPAR IVEC(J) WITH IT
          ZR    X0,FOUND
          SB6   B6+B4          B6= JL = JL+INC
          GE    B6,B3,IUHUNT
          SA2   A2+B4          X2= IVEC(J)
          EQ    LOOP
*
 INCZER   BX0   X2-X1
          ZR    X0,FOUND           IF IVEC(1)=IT
          EQ    IUHUNT
          END
#ifdef CERNLIB_TCGEN_IUHUNT
#undef CERNLIB_TCGEN_IUHUNT
#endif
