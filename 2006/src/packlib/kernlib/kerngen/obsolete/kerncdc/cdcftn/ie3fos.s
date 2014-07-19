*
* $Id: ie3fos.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: ie3fos.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT IE3FOS
*
* CERN PROGLIB# M220    IE3FOS          .VERSION KERNCDC  2.17  861204
*
          ENTRY IE3FOS
*
*     CALL IE3FOS (TAKE, PUT, NW, JBAD)
 
*         CONVERT 'TAKE' OF 'NW' CDC FLOATING POINT NUMBERS
*         TO 'PUT' IN IEEE REPRESENTATION, EACH 32 BITS RIGHT JUST.
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
          VFD   36/6HIE3FOS,24/IE3FOS
 IE3FOS   DATA  0
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
          ID    X1,BAD                test A indefinite
          MX3   1
          BX5   X1
          BX3   X3*X1         *X3= sign-bit of A
          AX5   60             X5= all zero or one
          BX4   X1-X5          X4= A = ABS(A)
          NX1   X4             X1= A normalized
          UX1   B4,X1          B4=N, X1=M(48 bits)  for  A = M * 2**N
          SX4   B7             X4= 1
          AX1   23             X1= M (25 bits)
          IX1   X1+X4          X1= M (25 bits) rounded
          BX5   X1                   to detect overflow to bit 26
          AX1   1              X1= M (24 bits)
          AX5   25            *X5= 1 if overflow to bit 26
          SB5   254            B5= 254
          SB4   X5+B4          B4= N bumped for rounding
          SB4   B4+173         B4= N+47 + 126 = IEEE ch'stic - 1
          LT    B4,B0,CHZERO        test IEEE ch'stic .LE. 0
          GE    B4,B5,OVF           test              .GE. 255
          SX4   B4+B7          X4= N+47 + 127 = IEEE characteristic
          MX0   37
          BX1   -X0*X1        *X1= IEEE mantissa
          LX3   32                  sign-bit positioned
          LX4   23                  ch'stic positioned for merge
          BX7   X1+X3
          BX7   X7+X4
 STORE    SA7   B1+B2               store PUT(JLOOP)
          LT    B2,B3,LOOP
          SA6   B6                  store JBAD to ouput parameter
          JP    IE3FOS
*
*--         denormalized output number
*
CHZERO    SB4   -B4            B4= 1 + IEEE ch'stic underflow
          SB5   24
          GE    B4,B5,STORE         zero if shift of 24 or more
          SB4   X5+B4               bumped by one if round to bit 26
          AX1   B4,X1          X1= mantissa shifted
          LX3   32                 sign-bit positioned
          BX7   X1+X3
          JP    STORE
*
*--         indefinite / overflow
*
 BAD      SA4   BADVAL         X4= IEEE indefinite
          JP    BADOVF
 OVF      SA4   OVFVAL         X4= IEEE overflow
          LX3   32                  sign-bit positioned
          BX4   X4+X3
 BADOVF   SX6   B2            *X6= JBAD = JLOOP
          BX7   X4
          JP    STORE
 BADVAL   DATA  17740177000B
 OVFVAL   DATA  17740000000B
          END
#ifdef CERNLIB_TCGEN_IE3FOS
#undef CERNLIB_TCGEN_IE3FOS
#endif
