*
* $Id: ibset.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: ibset.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT       IBSET
*
* CERN PROGLIB# M441    IBSET           .VERSION KERNCDC  2.18  870602
* ORIG. ?
*
*     FUNCTION IBSET(I,N)
*
*      COMPASS VERSION OF ANSI/ISA FUNCTION IBSET
*      I IS AN INTEGER VARIABLE IN THE CALLING SEQUENCE
*      N IS THE POSITION OF THE BIT TO BE SET
*
      ENTRY       IBSET
          VFD         30/5HIBSET,30/IBSET
 IBSET     DATA        0
          SA2         A1+1
          SA2         X2
          SA1         X1
          SX3         1
          SB2         X2
          LX3         B2,X3
          BX6         X3+X1
          EQ          B0,B0,IBSET
      END
#ifdef CERNLIB_TCGEN_IBSET
#undef CERNLIB_TCGEN_IBSET
#endif
