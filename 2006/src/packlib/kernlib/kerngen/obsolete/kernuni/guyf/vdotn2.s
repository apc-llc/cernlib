*
* $Id: vdotn2.s,v 1.1.1.1 1996/02/15 17:54:06 mclareni Exp $
*
* $Log: vdotn2.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:06  mclareni
* Kernlib
*
*
. *************************************
.
.     FUNCTION VDOTN2( A,B,N )                   *** F 121 ***
.     VDOTN2 = VDOTN( A,B,N) **2 = ( VDOT(A,B,N)/VMOD(A,N)*VMOD(B,N) )**2
. *   ERROR EXIT IF  N.LE.0   (==>VDOTN2=0.)
.
. *************************************
 
          AXR$
          FORT$
$(1),FIRS01
          +         'VDOTN2'
VDOTN2*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A), LOC(B)
          L         A0,2,XARG         . LOC( N )
          L         R1,0,A0           .  N
          S         X11,R3
          JGD       R1,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LE.0
          LXI,U     A2,1
          LXI,U     A3,1
          DS        A2,A15+1
          S         R1,R2
          LMJ       X11,VMD1
          L         A2,A3
          L         R1,R2
          S         A0,A3
          LMJ       X11,VMD1
          FM        A0,A3
          L         X11,R3
          JZ        A0,RETUR3
          DL        A2,A15+1
          S         A0,R1
          SZ        A0
VDT20
          L         A4,0,*A2
          FM        A4,0,*A3
          FA        A0,A4
          JGD       R2,VDT20
          FM        A0,A0             . VOT**2
          FD        A0,R1
          J         RETUR3
VMD1                                   . X**2
          SZ        A0
VMD2
          LMA       A4,0,*A2
          FM        A4,A4
          FA        A0,A4
          JGD       R1,VMD2
          J         0,X11
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.         DIAGNOSTIC PACKETS
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO  010  3
$(3)
DIAG1     +         0775003,VDOTN2
          +         0,DYN$
          +         'VDOTN2'
          ON    GERMAX
DIAG2     +         0776702,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VDOTN2( A,B,N ) ERROR: N.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON    GERMAX=0
DIAG2     +         0776202,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VDOTN2
#undef CERNLIB_TCGEN_VDOTN2
#endif
