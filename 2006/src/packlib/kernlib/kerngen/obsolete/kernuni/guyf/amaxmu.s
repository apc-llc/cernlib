*
* $Id: amaxmu.s,v 1.1.1.1 1996/02/15 17:54:02 mclareni Exp $
*
* $Log: amaxmu.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:02  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     FUNCTION AMAXMU( A, IDO, IW, NA )          *** E 103 ***
.     LOOKS FOR THE LARGEST ABSOLUTE VALUE IN THE SCATTERED VECTOR A
.     OF NORMALIZED FLOATING-POINT NUMBERS (OR INTEGERS, BUT NOT A MIXTURE)
.     A IS COMPOSED OF IDO SUBSETS, EACH ONE IW WORDS LONG
.     NA IS THE DISTANCE BTWEEN THE FIRST WORD OF TWO SUBSETS
. *   ERROR EXIT GERRB$ IF IDO.LE.0 .OR. IW.LE.0
. *   INTEGER OR SINGLE PRECESION A
 
.     FUNCTION LMAXMU(A,IDO,IW,NA)
.     RETURNS THE INDEX VALUE J OF A(J) FOUND BY AMAXMU(A,IDO,IW,NA)
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
SAVLOCA   EQU       A15+1
SAVIWM1   EQU       A15+2
$(1),FIRS01
                    'LMAXMU'
LMAXMU*
          L,U       R3,1
          J         AMAX1
                    'AMAXMU'
AMAXMU*
          SZ        R3
AMAX1
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A2,0,XARG          . LOC(A),LOC(IDO)
          DL        A0,2,XARG
          L         R1,0,A3           . IDO
          L         A3,0,A0           . IW
          L         A0,0,A1            . NA
          ANU       A0,A3             . (A1) = NA-IW
          JGD       R1,$+2            . IDO-
          J         LMJER1            . ERROR EXIT IF IDO.LE.0
          JGD       A3,$+2
LMJER1    SLJ       GERR$             . ERROR EXIT IF IW.LE.0
          TNZ       A0
          SZ        R1                . IF( NA=0 ) IDO = 1
          LXI,U     A2,1
          DS        A2,SAVLOCA
          LMA       A5,0,*A2
LMX10                          . A NEW CANDIDATE
          L         A4,A5             . CURRENT AMAXMU
          L         A0,A2             . CURRENT LMAXMU
          J         LMX20
LMX14                          . LOOP ON IDO SUBSETS
          A         A2,A1
          L         A3,SAVIWM1
LMX16
          LMA       A5,0,*A2
          TLE       A4,A5
          J         LMX10
LMX20     JGD       A3,LMX16
          JGD       R1,LMX14
          TNZ       R3
          J         LMX30
          AN        A0,SAVLOCA        . END LMAXMU
          J         RETUR4
LMX30
          L         A0,A4             . AMAXMU
          J         RETUR4
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
. ------- DIAGNOSTICS PACKETS FOR  FORT ----------
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010   3
$(3)
DIAG1     F         077,050,3,LMAXMU
          +         0,DYN$
                    'LMAXMU'
DIAG12    F         077,050,3,AMAXMU
          +         0,DYN$
                    'AMAXMU'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'AMAXMU( A,IDO,IW,N )ERROR: IDO.LE.0 OR IW.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_AMAXMU
#undef CERNLIB_TCGEN_AMAXMU
#endif
