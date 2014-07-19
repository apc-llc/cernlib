*
* $Id: vblank.s,v 1.1.1.1 1996/02/15 17:54:02 mclareni Exp $
*
* $Log: vblank.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:02  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE  VBLANK( A,N )                  *** F 121 ***
.     SUBROUTINE VZERO(A,N)
.     SUBROUTINE VFILL(A,N,STUFF)
.     SET 'BLANK','ZERO' OR STUFF IN N CONSECUTIVE LOCATIONS OF ARRAY A
.     DO NOTHING IF N.EQ.0
. *   SINGLE PRECISION VARIABLE A
. *   ERROR EXIT GERRB$ IF N.LT.0
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
 
#include "gerr_c.inc"
 
$(1),FIRS01
          +         'VFILL '
VFILL*
#include "ftnors1.inc"
          L         A2,2,XARG          . LOC(STUFF)
 DO FOR , L,U       A3,1
          L         A5,0,A2            . STUFF
          J         VZR2
          +         'VBLANK'
VBLANK*
          L         A5,BLANK
          J         VZR1
          +         'VZERO '
VZERO*
          SZ        A5
VZR1
#include "ftnors1.inc"
 DO FOR , L,U       A3,0
VZR2
#include "spyuse.inc"
* Ignoring t=pass
          DL        A0,0,XARG          . LOC(A), LOC(N)
          L,U       A2,A5
          L         A1,0,A1            . N
          LXI,U     A0,1
 DO FOR , JZ        A1,VZR12
 DO FTN , JZ        A1,RETUR2          . DO NOTHING IF N=0
          TP        A1
LMJER1    SLJ       GERR$              . ERROR EXIT IF N.LT.0
          S         A1,R1
          BT        A0,0,*A2
 DO FOR ,VZR12 A    X11,A3
          J         RETUR2
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
F         FORM      6,6,6,18
          INFO      010 03
$(3)
          ON        GERMAX
DIAG1     F         077,050,3,VFILL
          +         0,DYN$
          +         'VFILL '
DIAG11    F         077,050,3,VBLANK
          +         0,DYN$
          +         'VBLANK'
DIAG21    F         077,050,3,VZERO
          +         0,DYN$
          +         'VZERO '
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'VZERO,VBLANK,VFILL( A,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG1     F         077,050,4,VZERO
          +         0,DYN$
          +         'VZER,BLK,FIL'
DIAG11    F         077,061,2,VFILL
          +         0,DIAG1
DIAG21    F         077,061,2,VBLANK
          +         0,DIAG1
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_VBLANK
#undef CERNLIB_TCGEN_VBLANK
#endif
#ifdef CERNLIB_TCGEN_VFILL
#undef CERNLIB_TCGEN_VFILL
#endif
#ifdef CERNLIB_TCGEN_VZERO
#undef CERNLIB_TCGEN_VZERO
#endif
