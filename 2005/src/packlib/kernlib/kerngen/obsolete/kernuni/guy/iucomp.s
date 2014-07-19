*
* $Id: iucomp.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: iucomp.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     FUNCTION  IUCOMP( IT, IVEC,N )             *** V 304 ***
.     COMPARE WORD IT TO  IVEC(1),...,IVEC(N)  IF N.GT.0
.     COMPARE WORD IT TO IVEC(N),...,IVEC(1)  IF N.LT.0
.     RETURNS  IUCOMP=J  AT FIRST MATCH  W=V(J)
.     RETURNS  IUCOMP=0  IF NO FIND  OR  N.EQ.0
. *   INTEGER OR SINGLE PRECISION  IT, IVEC
. *   THIS IS A BIT TO BIT COMPARISON  EXCEPTED IF IT=0
. *   WORKS IF  IT = +0 OR -0
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1)
IUCOMP*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A3,2,XARG         . LOC( N )
          DL        A1,0,XARG         . LOC( IT ), LOC( IVEC )
          L         A0,0,A3           . N
          L         A4,0,A1           . IT
          JZ        A0,RETUR3
          SMA       A0,R1
          JN        A0,IUC3
.                                      IT .NE. 0  AND  N.GT.0
          LXI,U     A2,1
          JZ        A4,IUC4
          SE        A4,0,*A2
IUC1      L         A0,R1              . NO FIND
IUC2      AN        A0,R1              . FORTRAN LOCATION OF RESULT
          J         RETUR3
.                                      IT .NE. 0  AND  N.LT.0
IUC3      A,U       A0,1               . -N+1
          AN        A2,A0
          LXI,U     A2,-1
          JZ        A4,IUC6
          L,U       A0,1
          SE        A4,0,*A2
          L,XU      R1,-1
          A         A0,R1
          J         RETUR3
.                                      IT = +0 OR -0  AND N.GT.0
IUC4      JGD       R1,IUC5            . N-1
IUC5      TNZ       0,*A2             . LOOP
          J         IUC2
          JGD       R1,IUC5
          J         IUC1               . NO FIND
.                                      IT = +0 OR -0  AND N.LT.0
IUC6      LN        A0,A0              . N-1
IUC7      TZ        0,*A2
          JGD       A0,IUC7
          A,U       A0,1               . FORTRAN LOCATION OF RESULT
          J         RETUR3
 
#if (defined(CERNLIB_WBACK))&&(defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
          INFO      010  3
$(3)      +         0776003,IUCOMP
          +         0,SAVX11
          +         'IUCOMP'
#include "gerr_c.inc"
SAVX11    RES       1
#endif
          END
#ifdef CERNLIB_TCGEN_IUCOMP
#undef CERNLIB_TCGEN_IUCOMP
#endif
