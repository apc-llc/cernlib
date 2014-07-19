*
* $Id: ibclr.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: ibclr.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT       IBCLR
*
* CERN PROGLIB# M441    IBCLR           .VERSION KERNCDC  2.18  870602
* ORIG. ?
*
*     FUNCTION IBCLR(I,N)
*
*      COMPASS VERSION OF ANSI/ISA FUNCTION IBCLR
*      I IS AN INTEGER VARIABLE IN THE CALLING SEQUENCE
*      N IS THE POSTION OF THE BIT TO BE CLEARED
*
      ENTRY       IBCLR
          VFD         30/5HIBCLR,30/IBCLR
 IBCLR     DATA        0
          SA2         A1+1
          SA2         X2
          SA1         X1
          SX3         1
          SB2         X2
          LX3         B2,X3
          BX6         -X3*X1
          EQ          B0,B0,IBCLR
      END
#ifdef CERNLIB_TCGEN_IBCLR
#undef CERNLIB_TCGEN_IBCLR
#endif
