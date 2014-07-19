*
* $Id: lorenf.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: lorenf.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT LORENF
*
* CERN PROGLIB# U102    LORENF          .VERSION KERNCDC  0.1   750820
*
          ENTRY LORENF
*         CALL LORENF (U,PS,PI,PF)
*
          VFD   36/6HLORENF,24/LORENF
 LORENF   BSS   1
          SB7   1
          SA5   X1             X5= U
          SA2   A1+B7
          SB2   X2
          SA3   A2+B7
          SB6   3
          SB3   X3
          SA2   A3+B7
          SA4   B2+B6          X4= S4
          SB4   X2
          SA1   ONE                FLOATING 1
          IX7   X4-X5              S4-U
          SA2   B3+B6          X2= I4
          ZR    X7,CMS
*
          FX0   X1/X5          X0= 1/U
          SA3   A4-B7          X3= S3
          FX6   X2*X4          X6= S4*I4
          FX5   X4+X5          X5= S4+U
          SA4   A2-B7          X4= I3
          NX5   X5
          SA2   A3-B7          X2= S2
          FX7   X3*X4          X7= S3*I3
          SA4   A4-B7          X4= I2
          FX6   X6-X7              SUM
          FX7   X2*X4          X7= S2*I2
          NX6   X6
          FX5   X1/X5          X5= 1/S4+U
          SA1   B2             X1= S1
          FX6   X6-X7              SUM
          SA4   B3             X4= I1
          FX7   X1*X4          X7= S1*I1
          NX6   X6
          SA4   B3+B6          X4= I4
          FX6   X6-X7              SUM
          NX6   X6
          FX7   X6*X0          X7= F4
          SA7   B4+B6
          FX0   X7+X4          X0= F4+I4
          NX0   X0
          SA4   B3             X4= I1
          FX0   X0*X5          X0= FN
          SA5   B3+B7          X5= I2
          FX6   X1*X0          X6= S1*FN
          FX7   X2*X0          X7= S2*FN
          FX6   X4-X6          X6= I1-S1*FN
          SA4   A5+B7          X4= I3
          NX6   X6
          FX7   X5-X7          X7= I2-S2*FN
          SA6   B4
          NX7   X7
          SA7   A6+B7
          FX7   X3*X0          X7= S3*FN
          FX6   X4-X7          X6= I3-S3*FN
          NX6   X6
          SA6   A7+B7
          EQ    LORENF
 CMS      SA1   A2-B7          X1= I3
          BX7   X2             X7= I4
          SA4   A1-B7          X4= I2
          BX6   X1             X6= I3
          SA3   B3             X3= I1
          SA7   B4+B6
          SA6   A7-B7
          BX7   X4             X7= I2
          BX6   X3             X3= I1
          SA7   A6-B7
          SA6   B4
          EQ    LORENF
*
 ONE      DATA  1.
          END
#ifdef CERNLIB_TCGEN_LORENF
#undef CERNLIB_TCGEN_LORENF
#endif
