*
* $Id: iumode.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: iumode.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT IUMODE
*
* CERN PROGLIB# M506    IUMODE          .VERSION KERNCDC  1.10  790406
*
          ENTRY IUMODE
*
*     J= IUMODE(WORD)          RETURNS ZERO IF INTEGER
*                              31-MARCH-79,FJ
*
          VFD   36/6HIUMODE,24/IUMODE
IUMODE    DATA  0
          SA2   X1             X2= WORD
          AX2   48             X2= 49/SIGN , 11/EXP
          BX3   X2
          AX3   11             X3= 60/SIGN
          BX6   -X3*X2         X6= EXP IF +VE OR ZERO
          BX3   -X2*X3         X3= -EXP IF -VE OR ZERO
          BX6   X6+X3          X6= ABS(EXP)
          JP    IUMODE
          END
#ifdef CERNLIB_TCGEN_IUMODE
#undef CERNLIB_TCGEN_IUMODE
#endif
