*
* $Id: ublank.s,v 1.1.1.1 1996/02/15 17:54:01 mclareni Exp $
*
* $Log: ublank.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:01  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE UBLANK(A,JL,JR)                 *** V300 ***
.     SUBROUTINE UZERO(A,JL,JR)
.     SUBROUTINE UFILL(A,JL,JR,STUFF)
.     SET 'BLANK','ZERO' OR STUFF IN CONSECUTIVE LOCATIONS
.     A(JL),THRU,A(JR) OF ARRAY A
. *   IF JL.LE.0 SET JL=1
. *   ERROR EXIT GERRB$ IF JR.LT.JL
. *   SINGLE PRECISION VARIABLES A,STUFF
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
#include "gerr_c.inc"
 
$(1),FIRS01
          +         'UFILL '
UFILL*
          L         A2,3,XARG          . LOC(STUFF)
 DO FOR , L,U       A4,1
          L         A5,0,A2            . STUFF
          J         UZR2
          +         'UBLANK'
UBLANK*
          L         A5,BLANK
          J         UZR1
          +         'UZERO '
UZERO*
          SZ        A5
UZR1
 DO FOR , L,U       A4,0
UZR2
          DL        A1,1,XARG          . LOC(JL), LOC(JR)
          L,U       A3,A5
          L         A1,0,A1            . JL
          JGD       A1,$+2
          SZ        A1                 . MAX0(1,JL) - 1
          ANU       A1,0,A2            . (JL-1) - JR
          A,H2      A1,0,XARG          . LOC(A(JL))
          TN        A2
LMJER1    SLJ       GERR$              . ERROR EXIT IF JR.LT.MAX0(1,JL)
          SMA       A2,R1
          LXI,U     A1,1
 DO FOR , A         X11,A4
          BT        A1,0,*A3
          J         RETUR3
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,4
          +         0,DYN$
          +         'UZEROPACKAGE'
          ON        GERMAX
DIAG2     F         077,062,2,0
          +         LMJER1-FIRS01,DIAG1
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'UBLANK,FILL,ZERO( A,JL,JR,) ERROR: JR<MAX0(1,JL)'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
 END
#ifdef CERNLIB_TCGEN_UBLANK
#undef CERNLIB_TCGEN_UBLANK
#endif
#ifdef CERNLIB_TCGEN_UFILL
#undef CERNLIB_TCGEN_UFILL
#endif
#ifdef CERNLIB_TCGEN_UZERO
#undef CERNLIB_TCGEN_UZERO
#endif
