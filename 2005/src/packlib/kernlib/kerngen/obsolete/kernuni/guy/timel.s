*
* $Id: timel.s,v 1.1.1.1 1996/02/15 17:54:01 mclareni Exp $
*
* $Log: timel.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:01  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE  TIMEL( T )                     *** Z007 ***
.     RETURNS THE VALUE ( SECONDS) OF SUP TIME STILL AVAILABLE FOR THE RUN
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
 
AD        EQU       03
AP        EQU       013
 
#if defined(CERNLIB_U1108)
SLATE     INFO  2  2
$(2)
PCT       RES       AP+1
 
#endif
$(1)
TIMEL*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
#if defined(CERNLIB_U1110)
          LXI,U     A1,PCTBD$
          L,U       A2,RPCTA$
          LDJ       A1,$+1
          L         A4,AD,A2           . TIME LIMIT
          L,U       A3,0233
          AN        A4,AP,A2           . TIME LEFT
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
          L         A4,PCT+AD          . TIME LIMIT
          L,U       A3,0233
          AN        A4,PCT+AP          . TIME LEFT
          LCF       A3,A4
          FD        A4,( 5000.0)
          S         A4,0,A2            . T
#endif
          J         RETUR1
          END
#ifdef CERNLIB_TCGEN_TIMEL
#undef CERNLIB_TCGEN_TIMEL
#endif
