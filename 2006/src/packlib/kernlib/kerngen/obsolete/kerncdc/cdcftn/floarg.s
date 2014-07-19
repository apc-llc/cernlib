*
* $Id: floarg.s,v 1.1.1.1 1996/02/15 17:51:19 mclareni Exp $
*
* $Log: floarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:19  mclareni
* Kernlib
*
*
          IDENT  FLOARG
*
* CERN PROGLIB# M250    FLOARG          .VERSION KERNCDC  1.13  820111
*
          ENTRY  FLOARG
*     FUNCTION FLOARG(I)
 FLOARG   CON    0
          SA2    X1          .X2=I
          SB2    12
          MX0    1
          BX3    X2*X0       .X3=BIT 59 DE I
          SB4    48          .
          LX4    X0,B4
          BX5    X2*X4       .X5=BIT 47 DE I
          LX5    X5,B2
          IX6    X3-X5
          NZ     X6,EXIT
          PX3    X2
          NX2    X3
 EXIT     BX6    X2
          EQ     FLOARG
          END
#ifdef CERNLIB_TCGEN_FLOARG
#undef CERNLIB_TCGEN_FLOARG
#endif
