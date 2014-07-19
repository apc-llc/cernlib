*
* $Id: upkbyt.s,v 1.1.1.1 1996/02/15 17:54:02 mclareni Exp $
*
* $Log: upkbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:02  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE UPKBYT(A,JA,IY,N,MPACK)         *** M 422 ***
.     UNPACKS THE VECTOR A OF SMALL INTEGERS STARTING AT JA
.     INTO THE VECTOR IY
.     DEFINITION OF MPACK. SEE MANUAL/MPACK(1)=NBITS,MPACK(2)=INWORD/
.     DO NOTHING IF N.EQ.0
.     IF NBITS=0 USES NBITS=1,INWORD=36
. *   ERROR EXIT GERRB$ IF N.LT.0 OR JA.LE.0
. *   DOES NOT CHECK NBITS WITHIN(1,36) AND INWORD WITHIN(1,36/NBITS)
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(0)
PKBA      +  1
          + 36
$(1),FIRS01
          +         'UPKBYT'
UPKBYT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A1,4,XARG          . LOC(MPACK)
          L         A3,1,XARG          . LOC(JA)
          DL        A4,0,A1            . NBITS, INWORD
          L         A1,3,XARG          . LOC(N)
          L         A3,0,A3            . JA
          LN        A1,0,A1            . -N
          SZ        A2
          JZ        A1,RETUR5          . DO NOTHING IF N=0
          JP        A1,LMJER2          . ERROR EXIT IF N.LT.0
          JGD       A3,$+2             . JA-1
LMJER2     SLJ       GERR$             . ERROR EXIT IF JA.LE.0
          TNZ       A4
          DL        A4,PKBA
          DI        A2,A5              . JW-1, JB-1
          A,U       A1,2
          AN,U      A5,1               . INWORD-1
          LXI,U     A1,1
          A,H2      A2,0,XARG          . LOC(A(BYTE JA))
          S         A5,R2              . BACKUP FOR LOOP COUNTER
          AN        A5,A3              . FIRST VALUE OF LOOP COUNTER
          L         A0,2,XARG          . LOC(IY)
          MSI       A3,A4
          S         A5,R1
          L         A5,0,A2            . A(JW)
          LXI,U     A2,1
          SSL       A5,0,A3            . RIGHT ADJUST THE BYTES TO UNPACK
          LXI,U     A0,1
          L         A3,A4              . NBITS
          SZ        A4
UPK40                                  . LOOP
          DSC       A4,0,A3            . EXTRACT BYTE
          LSSC      A4,0,A3            . RIGHT ADJUST BYTE
          S         A4,0,*A0
          JMGI      A1,RETUR5
          JGD       R1,UPK40
          L         A5,1,*A2
          L         R1,R2
          J         UPK40
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG11    F         077,050,3,UPKBYT
          +         0,DYN$
          +         'UPKBYT'
          ON        GERMAX
DIAG2     +         0776702,0
          +         LMJER2-FIRS01,DYN$
DIAG13    F         077,047,LDIA13,LMJER2+1
          +         LDIA13-2,DIAG2
 'UPKBYT( A,JA,IY,N,MPACK ) ERROR: N<0 OR JA.LE.0'
LDIA13    EQU       $-DIAG13
          ON        GERMAX=0
DIAG12    F         077,062,02,LMJER2+1
          +         LMJER2-FIRS01,DIAG11
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_UPKBYT
#undef CERNLIB_TCGEN_UPKBYT
#endif
