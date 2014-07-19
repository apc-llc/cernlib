*
* $Id: locf.s,v 1.1.1.1 1996/02/15 17:54:00 mclareni Exp $
*
* $Log: locf.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:00  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     FUNCTION LOCF ( X )                        *** N 100 ***
.     RETURNS THE LOCATION OF X ( MACHINE ADDRESS )
.     ENTRY NAMES LOC, LOCF, XLOC
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
LOCF*
XLOC*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A0,0,XARG          . LOC(X)
          LXI,U     A0,0
          J         RETUR1
 
#if (defined(CERNLIB_WBACK))&&(defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
          INFO      010  3
$(3)      +         0776003,LOCF
          +         0,SAVX11
          +         'LOCF  '
#include "gerr_c.inc"
SAVX11    RES       1
#endif
          END
#ifdef CERNLIB_TCGEN_LOCF
#undef CERNLIB_TCGEN_LOCF
#endif
