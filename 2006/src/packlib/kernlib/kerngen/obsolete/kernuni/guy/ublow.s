*
* $Id: ublow.s,v 1.1.1.1 1996/02/15 17:54:01 mclareni Exp $
*
* $Log: ublow.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:01  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE UBLOW(IT,IV,N)                  *** M 409 ***
.     DISPERSE N FIRST CHARACTERS OF TEXT IT IN N CONSECUTIVE
.     LOCATIONS OF VECTOR IV USING A1 FORMAT
.     (IDENTICAL TO DECODE(F,IT)IV WITH F=(N A1))
.     DO NOTHING IF  N.EQ.0
. *   ERROR EXIT  GERRB$  IF  N.LT.0
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
 
UBL*      PROC      0
          LDSC      A4,NBITCH
          SSC       A4,NBITCH
          S         A4,0,*A1
          END  .    UBL
 
#include "gerr_c.inc"
 
$(1),FIRS01
          +         'UBLOW '
UBLOW*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
#if defined(CERNLIB_GUYFORE)||defined(CERNLIB_GUYFORT)
.     THIS ROUTINE UBLOW HAS THE SHORTEST CODE
.     BUT IS NOT THE FASTEST ROUTINE
.     BECAUSE OF THE JMGI TEST IN THE LOOP
 
          L         A2,2,XARG
          DL        A0,0,XARG
          LN        A2,0,A2           . - N
          L         A4,BLANK
          JZ        A2,RETUR3         . DO NOTHING IF  N .EQ. 0
          TN        A2
LMJER1    SLJ       GERR$             . ERROR EXIT IF  N.LT.0
          A,U       A2,2
          LXI,U     A0,1
          LXI,U     A1,1
          LXI,U     A2,1
UBLW12
          L         A5,0,*A0
          L,U       R1,NCHWD-1
UBLW14
          UBL
          JMGI      A2,RETUR3
          JGD       R1,UBLW14
          J         UBLW12
#endif
#if defined(CERNLIB_GUYFTN)
.     INTERMEDIATE LENGTH, INTERMEDIATE SPEED
.     CODED FOR FTN, USE  DI  INSTEAD OF  DSL  FOR E OR T
.
.         L         A2,2,XARG
.         DL        A0,0,XARG
.         L         A2,0,A2           . N
.         L         A4,BLANK
.         JZ        A2,RETUR3         . DO NOTHING IF N.EQ.0
.         JGD       A2,$+2
. LMJER1  SLJ       GERR$             . ERROR EXIT IF N.LT.0
.         DSL       A2,2              . (N-1)/NCHWD
.         SSL       A3,34             . MOD( (N-1), NCHWD )
.         LXI,U     A0,1
.         LXI,U     A1,1
.         JGD       A2,UBLW18
. UBLW16   .  N LEFT .LE. NCHWD
.         L         R1,A3
.         LN,U      A3,0              . SET FLAG (A3 NEG)
.         J         $+2
. UBLW18     .   LOOP ON FULL WORDS
.         L         A5,0,*A0
.         L,U       R1,NCHWD-1
. UBLW20      .   LOOP ON CHARACTERS INSIDE ONE WORD
.         UBL
.         JGD       R1,UBLW20
.         JGD       A2,UBLW18
.         JP        A3,UBLW16
.         J         RETUR3
.     FASTEST ROUTINE BUT LONG CODE
          L         A2,2,XARG
          DL        A0,0,XARG
          L         A2,0,A2           . N
          L         A4,BLANK
          JZ        A2,RETUR3         . DO NOTHING IF N=0
          TP        A2
LMJER1    SLJ       GERR$             . ERROR EXIT IF N.LT.0
          DSL       A2,2              . N / NCHWD
          LXI,U     A0,1
          LXI,U     A1,1
          J         UBL30
UBL16                                 . LOOP ON COMPLETE WORDS
          L         A5,0,*A0
 DO NCHWD , UBL
UBL30
          JGD       A2,UBL16
          SSL       A3,34             . MOD( N, NCHWD )
          JZ        A3,RETUR3
          L         A5,0,A0           . LAST WORD TO UNPACK
UBL40
          J         *UBL40,A3
I DO NCHWD-1 , + UBL30-3*I
#endif
/
. *****$$$*****$$$*****$$$*****$$$------
.
.     SUBROUTINE UBUNCH(IV,IT,N)
.     COMPRESS N CONSECUTIVE WORDS OF ARRAY IV IN N CONSECUTIVE CHARACTERS OF
.     ARRAY IT, KEEPING THE FIRST CHARACTER OF IV(J) - LAST WORD PADDED
.     WITH 'BLANK'
.     CONTRARY OF UBLOW
.     IDENTICAL TO ENCODE(F,IV)IT WITH F=(N A1)
.     DO NOTHING IF  N.EQ.0
. *   ERROR EXIT  GERRB$  IF  N.LT.0
.
. $$$*****$$$*****$$$*****$$$*****
 
          +         'UBUNCH'
UBUNCH*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A3,2,XARG
          DL        A0,0,XARG
          LN        A3,0,A3            . -N
          JZ        A3,RETUR3          . DO NOTHING IF L.EQ.0
          TN        A3
LMJER2    SLJ       GERR$              . ERROR EXIT IF N.LT.0
          A,U       A3,2               . 2-N
          LXI,U     A0,1
          LXI,U     A1,1
          LXI,U     A3,1
UBC1
          L,U       A2,NCHWD-1
UBC2
          LSSL      A4,NBITCH
 DO FOR , A,S1      A4,0,*A0
 DO FTN , A,Q1      A4,0,*A0
          JMGI      A3,UBC3
          JGD       A2,UBC2
          S         A4,0,*A1
          J         UBC1
UBC3      MSI,U     A2,NBITCH
          L         A5,BLANK
          LDSL      A4,0,A2
          S         A4,0,A1
          J         RETUR3
 
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,UBLOW
          +         0,DYN$
          +         'UBLOW '
DIAG11    F         077,050,3,UBUNCH
          +         0,DYN$
          +         'UBUNCH'
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         0,DYN$
DIAG3     F         077,047,LDIAG3,0
          +         LDIAG3-2,DIAG2
 'UBLOW,UBUNCH( IV,IT,N ) ERROR: N<0'
LDIAG3    EQU       $-DIAG3
DIAG4     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG3
DIAG14    F         077,062,2,LMJER2+1
          +         LMJER2-FIRS01,DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
DIAG12    F         077,062,2,LMJER2+1
          +         LMJER2-FIRS01,DIAG11
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_UBLOW
#undef CERNLIB_TCGEN_UBLOW
#endif
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
