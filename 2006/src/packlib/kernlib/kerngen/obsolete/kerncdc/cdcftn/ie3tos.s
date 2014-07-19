*
* $Id: ie3tos.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: ie3tos.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT IE3TOS
*
* CERN PROGLIB# M220    IE3TOS          .VERSION KERNCDC  2.17  861204
*
          ENTRY IE3TOS
*
*     CALL IE3TOS (TAKE, PUT, NW, JBAD)
 
*         CONVERT 'TAKE' OF 'NW' IN IEEE REPRESENTATION, EACH 32 BITS
*         RIGHT JUSTIFIED, TO 'PUT' AS CDC FLOATING POINT NUMBERS
*         RETURN IN 'JBAD' THE POSITION OF LAST CONVERSION PROBLEM
*
*         REGISTER ASSIGNMENTS
*
*              X1  current word            B1  adr(PUT(0))
*              X2  next word pre-loaded    B2  JLOOP = 1,2,3,...
*              X3  sign-bit                B3  NW
*
*              X6  last JBAD saved         B6  adr(JBAD)
*              X7  result -> PUT           B7  =1
*
          VFD   36/6HIE3TOS,24/IE3TOS
 IE3TOS   DATA  0
          SB7   1             *B7= 1
*                              X1= adr(TAKE(1))
          SA2   A1+B7          X2= adr(PUT(1))
          SA3   A2+B7          X3= adr(NW)
          SA4   A3+B7          X4= adr(JBAD)
*
          SB6   -B7
          SB1   X2+B6         *B1= adr(PUT(0))
          SB2   B0            *B2= JLOOP = 0
          SA2   X1             X2= TAKE(1) pre-loaded
          SA3   X3             X3= NW
          SB6   X4            *B6= adr(JBAD)
          MX6   0             *X6= JBAD = 0
          SB3   X3            *B3= NW
*
 LOOP     BX1   X2             X1= A = TAKE(JLOOP)
          SB2   B2+B7         *B2= JLOOP
          SA2   A2+B7         *X2= TAKE(JLOOP+1) preloaded
          MX7   0             *X7= 0  in case A=0
          ZR    X1,STORE              test A=0
          MX5   37
          BX5   -X5*X1        *X5= IEEE mantissa
          LX1   28
          MX3   1
          BX3   X3*X1          X3= sign-bit, left justfied
          AX3   60            *X3= all zero / all one if -ve
          LX1   9
          MX4   52
          BX4   -X4*X1        *X4= IEEE characteristic, right justif.
          ZR    X4,CHZERO
          SB4   X4-255         B4= char'stic - 255
          EQ    B4,B0,BAD
*
          MX0   1
          LX0   24             X0  bring the hidden bit
          BX5   X0+X5          X5= CDC mantissa, right justf.
          SB4   B4+105         B4= CDC exp = ch-127-23
          PX7   B4,X5          X7= result, not yet normalized
          NX7   X7             X7= result, absolute value
          BX7   X3-X7          X7= result, true value
 STORE    SA7   B1+B2               store PUT(JLOOP)
          LT    B2,B3,LOOP
          SA6   B6                  store JBAD to ouput parameter
          JP    IE3TOS
*
*--         denormalized output number
*
CHZERO    SB4   -149           B4= CDC exp = -126 - 23
          PX7   B4,X5          X7= result, not yet normalized
          NX7   X7             X7= result, absolute value
          BX7   X3-X7          X7= result, true value
          JP    STORE
*
*--         indefinite / overflow
*
 BAD      ZR    X5,OVF             if mantissa zero
          SA4   BADVAL         X4= indefinite
          SX6   B2            *X6= JBAD = JLOOP
          BX7   X4
          JP    STORE
*
 OVF      SA4   OVFVAL         X4= overflow
          SX6   B2            *X6= JBAD = JLOOP
          BX7   X3-X4
          JP    STORE
 BADVAL   DATA  17770000000000000000B
 OVFVAL   DATA  37770000000000000000B
          END
#ifdef CERNLIB_TCGEN_IE3TOS
#undef CERNLIB_TCGEN_IE3TOS
#endif
