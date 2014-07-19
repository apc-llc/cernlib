*
* $Id: pkbyt.s,v 1.1.1.1 1996/02/15 17:54:00 mclareni Exp $
*
* $Log: pkbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:00  mclareni
* Kernlib
*
*
. *****$$$*****$$$*****$$$*****
.
.     THIS A SET OF FOUR ROUTINES
.     PKBYT PAKCS A VECTOR OF SMALL INTEGERS INTO A BYTE STRING
.     UPKBYT UNPACKS A BYTE STRING ONTO A VECTOR OF SMALL INTEGERS
.     JBYTPK EXTRACTS ONE BYTE OUT OF THE STRING
.     SBYTPK STORES ONE BYTE INTO THE STRING
.
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE PKBYT(IA,X,JX,N,MPACK)          *** M 422 ***
.     PACKS N-VECTOR IA (1 BYTE PER WORD)
.      INTO ARRAY X, BYTE LOCATIONS JX TO JX+N-1
. *   SEE NOTICE M 422 FOR JW,JB,INWORD
.     NOTATION JX = BYTE INDEX IN ARRAY X
.              JW = WORD INDEX IN ARRAY X
.              JB = BYTE INDEX IN WORD JW
.              JW = (JX-1)/INWORD+1
.              JB = JX-(JW-1)*INWORD
.              MPACK(1) = NBITS
.              MPACK(2) = INWORD
.     DOES NOT CHECK NBITS WITHIN (1,36) AND INWORD WITHIN(1,36/NBITS)
.     DO NOTHING IF N=0
.     IF NBITS=0 SETS NBITS=1, INWORD=36
. *   ERROR EXIT GERRB$ IF N.LT.0 OR JX.LE.0
.
. *****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
SAVX10    EQU       R2
SAVA6     EQU       R3
$(0)
PKBA      +1
          +36
$(1),FIRS01
          +         'PKBYT '
PKBYT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A3,4,XARG          . LOC(MPACK)
          DL        A1,2,XARG          . LOC(JX), LOC(N)
          DL        A3,0,A3            . NBITS, INWORD
          S         X10,SAVX10
          LN        A5,0,A2            . -N
          L         A2,0,A1            . JX
          JZ        A5,RETUR5          . DO NOTHING IF N=0
          JP        A5,LMJER1          . ERROR EXIT IF N.LT.0
          JGD       A2,$+2             . JX-1
LMJER1    SLJ       GERR$
          SZ        A1
          TNZ       A3
          DL        A3,PKBA            . IF NBITS=0 THEN NBITS=1, INWORD=36
          L         X10,A5
          DI        A1,A4              . (A1)=(JX-1)/INWORD=JW-1, (A2)=JB-1
          S         A6,SAVA6
          A,U       X10,2              . 2-N
          S         A4,R1
          A,H2      A1,1,XARG          . LOC(X(BYTE JX))
          AN,U      A4,1               . NORMAL VALUE OF LOOP-COUNTER=INWORD-1
          L         A0,0,XARG          . LOC(IA)
          AN        A4,A2              . FIRST VALVE OF LOOP COUNTER=INWORD-JB
          L         A6,0,A1            . X(JW)
          MSI       A2,A3
          LXI,U     X10,1
          SSC       A6,0,A2            . KEEP (JB-1) BYTES OF X(JW)
          LXI,U     A0,1
          LN        A2,A3              . -NBITS
          LXI,U     A1,1
          MSI       A2,R1              . -NBITS*INWORD
          JGD       R1,$+2             . INWORD-1 IN BACKUP FOR LOOP COUNTER
PKB40                                  . LOOP1-FILL A NEW WORD
          L         A6,0,A1            . X(I)
PKB42                                  . LOOP2-FETCH NEXT BYTE
          L         A5,0,*A0
          DSL       A5,0,A3
          JMGI      X10,PKB60
          JGD       A4,PKB42
          SSC       A6,36,A2           . ADJUST
          L         A4,R1
          S         A6,0,*A1
          J         PKB40
PKB60                                  . LAST WORD
          MSI       A4,A3
          L         X10,SAVX10
          A         A2,A4             . - NBITS * NBYTES STORED
          SSC       A6,36,A2
          S         A6,0,A1
          L         A6,SAVA6
          J         RETUR5
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,PKBYT
          +         0,DYN$
          +         'PKBYT '
          ON        GERMAX
DIAG0     F         077,067,2,0
          +         0,DYN$
DIAG00    F         077,047,LDIA01,0
          +         LDIA01-2,DIAG0
 'CURRENT MPACK VALUE NOT CHECKED'
LDIA01    EQU       $-DIAG00
DIAG01    F         077,062,2,0
          +         DIAG00
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG01
 'PKBYT(A,X,JX,N,MPAK) ERROR: N<0 OR JX.LE.0'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,02,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_PKBYT
#undef CERNLIB_TCGEN_PKBYT
#endif
