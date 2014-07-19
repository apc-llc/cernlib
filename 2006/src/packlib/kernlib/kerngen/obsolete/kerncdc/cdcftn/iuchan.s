*
* $Id: iuchan.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: iuchan.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT IUCHAN
*
* CERN PROGLIB# Y201    IUCHAN          .VERSION KERNCDC  1.04  780211
*
*         FUNCTION IUCHAN(X,XL,DX,NX)
*
*         PROGRAM P.RASTL, JULY 1971
*
          ENTRY IUCHAN
          VFD   36/6HIUCHAN,24/IUCHAN
 IUCHAN   BSS   1
          SA4   X1             X4= X
          SB7   1              B7= 1
          SA3   A1+B7          X3= ADR(XL)
          SA2   A3+B7          X2= ADR(DX)
          SA1   A2+B7          X1= ADR(NX)
          SX0   17204B
          SA2   X2             X2= DX
          SA3   X3             X3= XL
          SA1   X1             X1= NX
          LX0   45             X0= 1.0
          RX2   X0/X2          X2= 1./DX
          RX4   X4-X3          X4= X - XL
          SX7   16744B
          NX5   X4             X5= (X-XL) NORMALIZED
          RX6   X2*X5          X6= (X-XL)/DX
          LX7   45             X7= .000001  APPROX.
          RX2   X6+X7          X2= BINZ = (X-XL)/DX + .000001
          SX1   X1+B7          X1= NX+1
          UX3   B1,X2          X3= JBIN * 2**-B1  WITH B1 -VE NORMALLY
          SX5   B7             X5= 1
          AX2   59             X2= 0--0 IF BINZ +VE / 7--7 IF -VE
+GEB1,B0,*+1SKIPIFBINZENORMOUS
          LX6   B1,X3          X6= INTG(BINZ)
          IX3   X6+X5          X3= JBIN = BINZ + 1
          BX6   -X2*X3         X6= JBIN = MAX0 (0,JBIN)
          IX7   X6-X1
          BX3   X7             X3= JBIN - (NX+1)
          AX7   59             X7= 7--7 IF JBIN.LT.NX+1 / 0--0 .GE.NX+1
          BX6   X7*X3          X6= MIN0 (JBIN-NX-1,0)
          IX6   X6+X1          X6= JBIN = MIN0 (JBIN,NX+1)
          EQ    IUCHAN
          END
#ifdef CERNLIB_TCGEN_IUCHAN
#undef CERNLIB_TCGEN_IUCHAN
#endif
