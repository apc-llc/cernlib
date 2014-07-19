*
* $Id: incbyt.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: incbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     FUNCTION INCBYT( INC,X,JX,MPACK )          *** M 423 ***
.     ADDS INCREMENT INC TO BYTE JX OF BYTE-VECTOR X
.     RETURNS OVERFLOW VALUE IF (BYTE+INC).GT.MPACK(3)
. *   ERROR EXIT GERR$ IF NBITS.LE.0 .OR. JX.LE.0
. *   NOT PROTECTED FOR INWORD=0
. *   NBITS MUST BE .LE.35 (GENERALLY NBITS.LE.18 )
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(1),FIRS01
          +         'INCBYT'
INCBYT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A3,0,XARG          . LOC(INC), LOC(X)
          SZ        A1
          L         R1,0,A3            . INC
          DL        A2,2,XARG          . LOC(JX), LOC(MPACK)
          L         A0,0,A3            . NBITS
          L         A2,0,A2            . JX
          TG,U      A0,1               . TEST NBITS.GT.0
          JGD       A2,$+2
LMJER1    SLJ       GERR$              . ERROR EXIT IF JX.LE.0 .OR. NBITS.LE.0
          DI        A1,1,A3            . (A1)=(JX-1)/INWORD=JW-1,  (A2)=JB-1
          A,U       A2,1               . JB
          A         A1,A4              . LOC( X(JW) )
          MSI       A2,A0              . JB*NBITS
          L         A4,0,A1            . X(JW)
          LN        A0,A0              . -NBITS
          L         A3,2,A3            . MAX
          SSC       A4,0,A2            . ADJUST BYTE AT LEFT OF A4
          DSL       A4,36,A0           . PUSH RIGHT PART IN A5
          A         A4,R1              . BYTE+INC
          TLE       A3,A4              . TEST BYTE.GT.MAX
          J         INCB40
          SZ        A3
INCB20
          LDSL      A4,36,A0
          LSSC      A4,0,A2 .
          L         A0,A3
          SA        A4,0,A1 .
          J         RETUR4
INCB40
          AN        A4,A3              . LOST = BYTE-MAX
          DSC       A3,36              . BYTE = MAX
          J         INCB20
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,INCBYT
          +         0,DYN$
          +         'INCBYT'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'INCBYT(INC,X,JX,MPACK) ERROR: LX<1 OR MPACK(1)<1'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
 END
#ifdef CERNLIB_TCGEN_INCBYT
#undef CERNLIB_TCGEN_INCBYT
#endif
