*
* $Id: jbytpk.s,v 1.1.1.1 1996/02/15 17:54:00 mclareni Exp $
*
* $Log: jbytpk.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:00  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     FUNCTION JBYTPK( A,JA,MPACK )              *** M 422 ***
.     RETURNS BYTE JA OF BYTE-VECTOR A
. *   RESTRICTIONS : SEE JBIT
. *   IF MPACK(1)=0  SET NBITS=1, INWORD=36
. *   DOES NOT CHECK NBITS,INWORD
. *   ERROR EXIT GERR$ IF JA.LE.0
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(0)
PKBA      +  1
          + 36
$(1),FIRS01
          +         'JBYTPK'
JBYTPK*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,1,XARG          . LOC(JA), LOC(MPACK)
          L         A2,0,A2            . JA
          DL        A3,0,A3            . NBITS, INWORD
          SZ        A1
          JGD       A2,$+2             . JA-1
LMJER3    SLJ       GERR$              . ERROR EXIT IF JA.LE.0
          TNZ       A3
          DL        A3,PKBA            . IF NBITS=0( SET NBITS=1, INWORD=36
          DI        A1,A4              . (A1)=(JA-1)/INWORD=JW-1, (A2)=JB-1
          A,H2      A1,0,XARG          . LOC(A(JW))
          A,U       A2,1               . JB
          MSI       A2,A3              . JB*NBITS
          L         A0,0,A1            . A(JW)
          LN        A3,A3              . -NBITS
          SSC       A0,0,A2            . THE BYTE IS LEFT ADJUSTED
          SSL       A0,36,A3           . THEN IT IS RIGHT ADJUSTED
          J         RETUR3
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG21    F         077,050,3,JBYTPK
          +         0,DYN$
          +         'JBYTPK'
          ON        GERMAX
DIAG2     +         0776702,0
          +         LMJER3-FIRS01,DYN$
DIAG23    F         077,047,LDIA23,LMJER3+1
          +         LDIA23-2,DIAG2
 'JBYTPK( A,JA,MPACK ) ERROR: JA.LE.0'
LDIA23    EQU       $-DIAG23
          OFF
          ON        GERMAX=0
DIAG22    F         077,062,02,LMJER3+1
          +         LMJER3-FIRS01,DIAG21
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_JBYTPK
#undef CERNLIB_TCGEN_JBYTPK
#endif
