*
* $Id: dcheck.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: dcheck.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
. ----------------------------------------------------------------------
.     AREA 4 :
.     CODE FOR DEVICE-CHECK ROUTINE
 
.         SUBROUTINE DCHECK(UNIT,DEV)
.         INTEGER UNIT,DEV(2)
.         RETURNS DEVICE TYPE FOR FILE UNIT
.         DEV(1)             I DEV(2)
.         0 : NOT ASSIGNED   I NOT USED
.         1 : R.M.S (DISC)   I SIZE (WORDS)
.         2 : TAPE           I 7 OR 9 TRACKS
.         3 : OTHER TYPES    I EQUIPMENT TYPE
.
 
          AXR$
          FORT$
PF        FORM      12,6,18
TRKW      EQU       1792
POSW      EQU       64*TRKW
EQUIP     EQUF      FACPKT+6,,S1
GRANUL    EQUF      FACPKT+6,,S3
MXGRAN    EQUF      FACPKT+8,,H2
 
GERR$C    INFO  2  4
$(4)
BLANK     RES  1
DYN$      RES  2
FACPKT    RES  13
 
$(1),FIRS01
          +         'DCHECK'
DCHECK*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          DL        A1,0,XARG         . LOC(UNIT), LOC(DEV)
          L         A3,0,A1           . UNIT
          TG,U      A3,1
          TG,U      A3,NTBSZ$
          J         DVKERR             . NOT A POSSIBLE FORTRAN UNIT
          SLJ       BNDC2$
          S,H1      A3,FACPKT+1        . '   ' IN H1(FACPKT(2))
          L,U       A0,FACPKT
          S         A3,FACPKT
          LXI,U     A0,0377777         . REQUEST MAX-SIZE FOR FITEM$ PACKET
          S,H2      A3,FACPKT+1        . '   ' IN H2(FACPKT(2))
          ER        FITEM$
          L         A1,EQUIP
          JZ        A1,DVKEND          . FILE IS NOT ASSIGNED
          TG,U      A1,040
          J         DVKSYM             . A SYMBIONT OR COMMUNICATION FILE
          TG,U      A1,020
          J         DVKRMS             . A R.M.S FILE
 
          L,U       A5,9               . A NINE TRACK TAPE
          L,U       A4,2
          TE,U      A1,014
          L,U       A5,7               . A SEVEN TRACK TAPE
          DS        A4,0,A2
          J         RETUR2
 
DVKRMS
          L,U       A4,1
          L,U       A5,TRKW
          TZ        GRANUL
          L,U       A5,POSW
          MSI       A5,MXGRAN
          DS        A4,0,A2
          J         RETUR2
 
DVKSYM
          L,U       A4,3
          S         A1,1,A2            . EQUIPMENT TYPE IN DEV(2)
          S         A4,0,A2
          J         RETUR2
 
DVKEND
          SZ        0,A2
          J         RETUR2
 
DVKERR
          SZ        0,A2
LMJER1    SLJ       GERR$
 
.     DIAG PACKETS
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
          INFO  010  03
$(3),DIAG1 +        0775003,DCHECK
          +         DYN$
          +         'DCHECK'
          ON    GERMAX
F         FORM      6,6,6,18
DIAG2     +         0776702,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'DCHECK(LUN,.) LUN NOT WITHIN ALLOWED RANGE FOR FORTRAN UNITS'
LDIAG3    EQU       $-DIAG3
          OFF . GERMAX
          ON    GERMAX=0
DIAG2     +         0776202,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF . GERMAX=0
#endif
          END
