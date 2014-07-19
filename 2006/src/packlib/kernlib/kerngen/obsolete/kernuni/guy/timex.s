*
* $Id: timex.s,v 1.1.1.1 1996/02/15 17:54:01 mclareni Exp $
*
* $Log: timex.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:01  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE  TIMEX( T )                     *** Z007 ***
.     RETURNS SUP TIME SINCE THE BEGINNING OF THE RUN (SECONDS)
.     T = REAL, SINGLE PRECISION VARIABLE
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
AP        EQU       013      . CURRENT VALUE OF S.U.P COUNTER
 
#if defined(CERNLIB_U1108)
SLATE     INFO  2  2
$(2)
PCT       RES       AP+1
 
#endif
$(1)
TIMEX*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
#if defined(CERNLIB_U1110)
          LXI,U     A1,PCTBD$
          L,U       A2,RPCTA$
          LDJ       A1,$+1
          L         A4,AP,A2           . SUP
          L,U       A3,0233
          LDJ       A1,$+1
          LCF       A3,A4
          L         A0,0,XARG          . LOC(T)
          FD        A4,( 5000.0)
          S         A4,0,A0            . T
#endif
#if defined(CERNLIB_U1108)
          L         A2,0,XARG          . LOC(T)
          L,U       A0,PCT
          LXI,U     A0,AP+1
          ER        PCT$
          L,U       A3,0233
          LCF       A3,PCT+AP          . SUP
          FD        A4,( 5000.0)
          S         A4,0,A2            . T
#endif
          J         RETUR1
          END
#ifdef CERNLIB_TCGEN_TIMEX
#undef CERNLIB_TCGEN_TIMEX
#endif
