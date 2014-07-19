*
* $Id: intarg.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: intarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT  INTARG
*
* CERN PROGLIB# M250    INTARG          .VERSION KERNCDC  1.13  820111
*
          ENTRY  INTARG
*     FUNCTION INTARG(A)
 INTARG   CON    0
          SA2    X1          .X2=A
          SB2    12
          MX0    1
          BX3    X2*X0       .X3=BIT 59 DE A
          SB1    48
          LX4    X0,B1
          BX5    X4*X2       .X5=BIT 47 DE A
          LX5    X5,B2
          IX6    X3-X5
          ZR     X6,EXIT
          UX3    X2,B2
          LX2    X3,B2
 EXIT     BX6    X2
          EQ     INTARG
          END
#ifdef CERNLIB_TCGEN_INTARG
#undef CERNLIB_TCGEN_INTARG
#endif
