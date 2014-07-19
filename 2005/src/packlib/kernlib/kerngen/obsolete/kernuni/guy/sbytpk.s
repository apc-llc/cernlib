*
* $Id: sbytpk.s,v 1.1.1.1 1996/02/15 17:54:01 mclareni Exp $
*
* $Log: sbytpk.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:01  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     SUBROUTINE SBYTPK( A,X,JX,MPACK )          *** M 422 ***
.     COPIES BYTE AT 1 OF A INTO BYTE JX OF BYTE-VECTOR X
. *   ERROR EXIT GERRB$ IF JX.LE.0
. *   IF MPACK(1)=0  SET NBITS=1, INWORD=36
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(0)
PKBA      +  1
          + 36
$(1),FIRS01
          +         'SBYTPK'
SBYTPK*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,2,XARG          . LOC(JX), LOC(MPACK)
          SZ        A1
          L         A2,0,A2            . JX
          DL        A3,0,A3            . NBITS, INWORD
          JGD       A2,$+2             . JX-1
LMJER4    SLJ       GERR$              . ERROR EXIT IF JX.LE.0
          TNZ       A3
          DL        A3,PKBA
          DI        A1,A4              . JW-1, JB-1
          A,H2      A1,1,XARG          . LOC(A(JW))
          MSI       A2,A3              . (JB-1)*NBITS
          L         A0,0,XARG          . LOC(A)
          L         A5,0,A1            . X
          L         A4,0,A0            . A
          SSC       A5,0,A2            . RIGHT ADJUST BYTE
          A         A2,A3              . JB*NBITS
          DSL       A4,0,A3            . PUSH A-BYTE INTO X
          LSSC      A5,0,A2            . RE-ORDER
          S         A5,0,A1
          J         RETUR4
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG31    F         077,050,3,SBYTPK
          +         0,DYN$
          +         'SBYTPK'
          ON        GERMAX
DIAG2     +         0776702,0
          +         LMJER4-FIRS01,DYN$
DIAG33    F         077,047,LDIA33,LMJER4+1
          +         LDIA33-2,DIAG2
 'SBYTPK( A,X,JX,MPACK) ERROR: JX.LE.0'
LDIA33    EQU       $-DIAG33
          OFF
          ON        GERMAX=0
DIAG32    F         077,062,02,LMJER4+1
          +         LMJER4-FIRS01,DIAG31
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_SBYTPK
#undef CERNLIB_TCGEN_SBYTPK
#endif
