*
* $Id: timed.s,v 1.1.1.1 1996/02/15 17:54:01 mclareni Exp $
*
* $Log: timed.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:01  mclareni
* Kernlib
*
*
. **************************************
.
.     SUBROUTINE  TIMED( T )                     *** Z007 ***
.     RETURNS EXECUTION TIME INTERVAL SINCE LAST CALL TO TIMED
.
. **************************************
 
          AXR$
          FORT$
AP        EQU       013
 
#if defined(CERNLIB_U1108)
SLATE     INFO  2  2
$(2)
PCT       RES       AP+1
 
#endif
$(0)
LAST      +         0
 
$(1)
TIMED*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
#if defined(CERNLIB_U1110)
          LXI,U     A1,PCTBD$
          L,U       A2,RPCTA$
          LDJ       A1,$+1
          L         A4,AP,A2           . SUP
          LDJ       A1,$+1
          ANU       A4,LAST
          L,U       A2,0233
          S         A4,LAST
          LCF       A2,A5
          L         A0,0,XARG          . LOC(T)
          FD        A3,( 5000.0)
          S         A3,0,A0            . T
#endif
#if defined(CERNLIB_U1108)
          L         A2,0,XARG          . LOC(T)
          L,U       A0,PCT
          LXI,U     A0,AP+1
          ER        PCT$
          L         A4,PCT+AP          . SUP
          ANU       A4,LAST
          L,U       A3,0233
          S         A4,LAST
          LCF       A3,A5
          FD        A4,( 5000.0)
          S         A4,0,A2            . T
#endif
          J         RETUR1
          END
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
