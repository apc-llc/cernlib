*
* $Id: btest.s,v 1.1.1.1 1996/02/15 17:51:19 mclareni Exp $
*
* $Log: btest.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:19  mclareni
* Kernlib
*
*
          IDENT       BTEST
*
* CERN PROGLIB# M441    BTEST           .VERSION KERNCDC  2.18  870602
* ORIG. ?
*
*     LOGICAL FUNCTION BTEST(I,N)
*
*      COMPASS VERSION OF ANSI/ISA FUNCTION BTEST
*      I IS AN INTEGER VARIABLE IN THE CALLING SEQUENCE
*      N IS THE BIT TO BE TESTED
*
      ENTRY       BTEST
          VFD         30/5HBTEST,30/BTEST
 BTEST     DATA        0
          SA2         A1+1
          SA2         X2
          SA1         X1
          SX3         1
          SB2         X2
          LX3         B2,X3
          BX6         X3*X1
          ZR          X6,BTEST
          SX6         -1
          EQ          B0,B0,BTEST
      END
#ifdef CERNLIB_TCGEN_BTEST
#undef CERNLIB_TCGEN_BTEST
#endif
