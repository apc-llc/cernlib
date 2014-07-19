*
* $Id: ie3tod.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: ie3tod.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT IE3TOD
*
* CERN PROGLIB# M220    IE3TOD          .VERSION KERNCDC  2.17  861204
*
          ENTRY IE3TOD
*
*     CALL IE3TOD (TAKE, PUT, NDP, JBAD)
 
*         CONVERT 'TAKE' OF NW=2*NDP WORDS CONTAINING
*         'NDP' DOUBLE-PRECISION NUMBERS IN IEEE REPRESENTATION, EACH
*         32 BITS RIGHT JUSTIFIED, TO 'PUT' AS CDC D.P. NUMBERS
*         RETURN IN 'JBAD' THE POSITION OF LAST CONVERSION PROBLEM
*
*         REGISTER ASSIGNMENTS
*
*              X1  current word LH         B1  adr(PUT(0))
*              X2  next word pre-loaded    B2  JLOOP = 1,2,3,...
*              X3  sign-bit                B3  NW
*              X4  current word RH
*
*              X6  result LH -> PUT        B6  adr(JBAD)
*              X7  result RH -> PUT        B7  =1
*
          VFD   36/6HIE3TOD,24/IE3TOD
 IE3TOD   DATA  0
          SB7   1             *B7= 1
*                              X1= adr(TAKE(1))
          SA2   A1+B7          X2= adr(PUT(1))
          SA3   A2+B7          X3= adr(NDP)
          SA4   A3+B7          X4= adr(JBAD)
*
          SB6   -B7
          MX6   0
          SB1   X2+B6         *B1= adr(PUT(0))
          SB2   B0            *B2= JLOOP = 0
          SA2   X1             X2= TAKE(1) pre-loaded
          SA3   X3             X3= NDP
          SA6   X4                  preset  JBAD = 0
          LX3   1              X3= 2*NDP
          SB6   X4            *B6= adr(JBAD)
          SB3   X3            *B3= NW = 2*NDP
*
 LOOP     BX1   X2             X1= AL = TAKE(JLOOP)
          SA4   A2+B7         *X4= AR = TAKE(JLOOP+1)
          SB2   B2+B7         *B2= bump JLOOP by 1
          SA2   A4+B7         *X2= TAKE(JLOOP+2) preloaded
          MX6   0             *X6= 0  in case A=0
          MX7   0             *X7= 0  in case A=0
          ZR    X0,STORE           test AL = 0
*
          MX5   40
          BX5   -X5*X1        *X5= IEEE LH mantissa, 20 bits
          LX1   28
          MX3   1
          BX3   X3*X1          X3= sign-bit, left justfied
          AX3   60            *X3= all zero / all one if -ve
          LX1   12
          MX0   49
          BX1   -X0*X1        *X1= IEEE characteristic, right justif.
          SB4   X1-2047        B4= char'stic - 2047
          EQ    B4,B0,BAD
          SB5   -1022
          SB4   X1-1070        B4= CDC exp = IEEE ch - 1023 - 47
          LT    B4,B5,STORE        underflow
*
          LX5   27             X5= CDC Lmant from IEEE Lh, 20 bits
          LX4   55
          MX0   33
          BX0   -X0*X4         X0= CDC Lmant from IEEE Rh, 27 bis
          BX5   X5+X0         *X5= CDC Lmant, 47 bits
          MX0   5
          BX4   X0*X4
          LX4   48            *X4= CDC Rmant from IEEE Rh, 5 bits
          MX0   1                  bring the hidden bit
          LX0   48
          BX5   X0+X5         *X5= CDC Lmant, 48 bits
          PX6   B4,X5
          SB4   B4-48
          PX7   B4,X4
          BX6   X3-X6
          BX7   X3-X7
 STORE    SA6   B1+B2               store PUT(JLOOP)
          SA7   A6+B7               store PUT(JLOOP+1)
          SB2   B2+B7         *B2= bump JLOOP by 1 again
          LT    B2,B3,LOOP
          JP    IE3TOD
*
*--         indefinite / overflow
*
 BAD      ZR    X5,OVF             if mantissa zero
          SA4   BADVAL         X4= IEEE indefinite
          JP    BADOVF
 OVF      SA4   OVFVAL         X4= IEEE overflow
          BX4   X3-X4               add sign
 BADOVF   SX6   B2+B7          X6= JBAD = JLOOP/2
          AX6   1
          SA6   B6
          BX6   X4
          JP    STORE
 BADVAL   DATA  17770000000000000000B
 OVFVAL   DATA  37770000000000000000B
          END
#ifdef CERNLIB_TCGEN_IE3TOD
#undef CERNLIB_TCGEN_IE3TOD
#endif
