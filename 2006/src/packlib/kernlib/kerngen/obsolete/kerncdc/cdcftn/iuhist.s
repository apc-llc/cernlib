*
* $Id: iuhist.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: iuhist.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT IUHIST
*
* CERN PROGLIB# Y203    IUHIST          .VERSION KERNCDC  1.04  780211
*
*         FUNCTION IUHIST(X,PAR,SPILL)
*         PAR(1)=NX, PAR(2)=1./DX, PAR(3)=XL
*
*         PROGRAM P.RASTL, JULY 1971
*
          ENTRY IUHIST
          VFD   36/6HIUHIST,24/IUHIST
 IUHIST   BSS   1
          SA4   X1             X4= X
          SB7   1              B7= 1
          SA2   A1+B7          X2= ADR(PAR)
          SA5   A2+B7          X5= ADR(SPILL)
          SA2   X2+B7          X2= 1/DX = PAR(2)
          SA3   A2+B7          X3= XL = PAR(3)
          SA1   A2-B7          X1= NX = PAR(1)
          SX7   16744B
          RX4   X4-X3          X4= X - XL
          SB3   X5             B3= ADR(SPILL)
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
          MX0   59             X0= MASK FOR .TRUE. = 7--76
          IX7   X6-X1
          BX3   X7             X3= JBIN - (NX+1)
          AX7   59             X7= 7--7 IF JBIN.LT.NX+1 / 0--0 .GE.NX+1
          BX6   X7*X3          X6= MIN0 (JBIN-NX-1,0)
          BX7   -X7+X2         X7=   0--0 JBIN.LT.NX+1 / 7--7 .GE. NX+1
*                                  + 7--7 -VE BINZ     / 0--0 +VE BINZ
          IX6   X6+X1          X6= JBIN = MIN0 (JBIN,NX+1)
          BX7   X0*X7          X7= SPILL
          SA7   B3
          EQ    IUHIST
          END
#ifdef CERNLIB_TCGEN_IUHIST
#undef CERNLIB_TCGEN_IUHIST
#endif
