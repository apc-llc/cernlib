*
* $Id: reqpp.s,v 1.1.1.1 1996/02/15 17:47:43 mclareni Exp $
*
* $Log: reqpp.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:43  mclareni
* Kernlib
*
*
#if (defined(CERNLIB_CDC))&&(defined(CERNLIB_NOSBE))
          IDENT REQPP
          ENTRY REQPP
REQPP
          SA1  A1
          SB1  X1
+SA11
          NZ   X1,*
          SA1  B1
          BX6  X1
          SA6  1
          XJ
LOOP      SA2  X1
          LX2  59
          PL   X2,LOOP
          EQ   REQPP
          END
#endif
