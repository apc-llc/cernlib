*
* $Id: ie3fod.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: ie3fod.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT IE3FOD
*
* CERN PROGLIB# M220    IE3FOD          .VERSION KERNCDC  2.17  861204
*
          ENTRY IE3FOD
*
*     CALL IE3FOD (TAKE, PUT, NDP, JBAD)
 
*         CONVERT 'TAKE' OF NW=2*NDP WORDS CONTAINING
*         'NDP' CDC DOUBLE-PRECISION FLOATING POINT NUMBERS
*         TO 'PUT' IN IEEE REPRESENTATION, EACH 32 BITS RIGHT JUST.
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
          VFD   36/6HIE3FOD,24/IE3FOD
 IE3FOD   DATA  0
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
          ZR    X1,STORE              test A=0
          ID    X1,BAD                test A indefinite
          OR    X1,OVF                test A overflow
*
          MX3   1
          BX0   X1
          BX3   X3*X1         *X3= sign-bit of A
          AX0   60             X0= all zero or one
          BX1   X1-X0          X1= AL = ABS(AL)
          BX4   X4-X0          X4= AR = ABS(AR)
          UX1   B4,X1          B4=N, X1=M(48 bits)  for AL = M * 2**N
          SB4   B4+1024        B4= N+1024
          EQ    B4,B7,STORE         test underflow
          SB4   B4+46         *B4= N+47 + 1023 = IEEE char.
          SB5   2047
          GE    B4,B5,OVF           test IEEE char. .GE. 2047
*
          UX4   B0,X4          X4= 48 bits of DP mantissa with sign
          MX0   12
          BX4   -X0*X4         X4= (48) bits 49-96 of DP mant. only
          AX4   43             X4= (5)  bits 49-53 of DP mant.
          BX7   X1             X7= (48) bits  1-48 of DP mant.
          MX0   33
          BX7   -X0*X7         X7= (27) bits 22-48 of DP mant.
          LX7   5
          BX7   X7+X4         *X7= (32) bits 22-53 of IEEE mant.
*
          AX1   27             X1= (21) bits  1-21 of mant.
          MX0   40
          BX1   -X0*X1         X1= (20) bits  2-21 of IEEE mant
          SX4   B4             X4= IEEE char
          LX3   32                  sign-bit positioned
          LX4   20                  char. positioned for merge
          BX6   X1+X3
          BX6   X6+X4
 STORE    SA6   B1+B2               store PUT(JLOOP)
          SA7   A6+B7               store PUT(JLOOP+1)
          SB2   B2+B7         *B2= bump JLOOP by 1 again
          LT    B2,B3,LOOP
          JP    IE3FOD
*
 BAD      SA4   BADVAL         X4= IEEE indefinite
          JP    BADOVF
 OVF      SA4   OVFVAL         X4= IEEE overflow
          LX3   32                  sign-bit positioned
          BX4   X4+X3
 BADOVF   SX6   B2+B7          X6= JBAD = JLOOP/2
          AX6   1
          SA6   B6
          BX6   X4
          JP    STORE
 BADVAL   DATA  17774017700B
 OVFVAL   DATA  17774000000B
          END
#ifdef CERNLIB_TCGEN_IE3FOD
#undef CERNLIB_TCGEN_IE3FOD
#endif
