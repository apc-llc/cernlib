*
* $Id: uopt.s,v 1.1.1.1 1996/02/15 17:54:02 mclareni Exp $
*
* $Log: uopt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:02  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     SUBROUTINE UOPT(DAT,POSS,IOPT,N)           *** M 502 ***
.     POSS IS A STRING OF N POSSIBLE OPTIONS.
.     DAT IS THE STRING OF OPTIONS YOU ACTUALLY WANT TO STEER
.     IOPT IS A N-VECTOR
.     IOPT(J)=0 OR 1
.     DEPENDING ON THE ABSCENCE OR PRESENCE OF CHARACTER NUMBER J
.     OF POSS IN THE STRING DAT
. *   ERROR EXIT IF  N.LE.0 .OR. N.GT.30
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
MXOPT     EQU       30
 
SLATE     INFO 2 2
$(2)
UOPTB     RES       MXOPT
 
$(1),FIRS01
          +         'UOPT  '
UOPT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
.                            *****     CHECK  N  WITHIN RANGE
.                            *****     CALL  VZERO( IOPT,N )
          DL        A2,2,XARG         . LOC(IOPT), LOC(N)
          L,U       A1,A4
          SZ        A4
          L         A3,0,A3           . N
          LXI,U     A2,1
          TG,U      A3,1
          TG,U      A3,MXOPT+1
LMJER1    SLJ       GERR$             . ERROR EXIT IF N.LE.0 .OR. N.GT.30
          S         A3,R1
          BT        A2,0,*A1          . VZERO( IOPT, N )
          AN,U      A2,1
          DL        A0,0,XARG         . LOC(DAT), LOC(POSS)
          DS        A2,R2             . SAVE LOC( IOPT(N) ),  N
.                            *****     CALL UBLOWZ( POSS,UOPTB,N )
          LXI,U     A1,1
          L,U       A2,UOPTB
          L,U       A3,2
          LXI,U     A2,1
          AN        A3,R3             . 2-N
          LXI,U     A0,1
          LXI,U     A3,1
UOPT40
          L         A4,0,*A1          . NEXT WORD TO UNPACK
          L,U       R1,NCHWD-1
UOPT42
          SZ        A5
          LDSC      A4,NBITCH
          S         A5,0,*A2
          JMGI      A3,UOPT60         . END OF BLOWZ
          JGD       R1,UOPT42
          J         UOPT40
.                            *****     LOOP
. EXTRACT SUCCESSIVE CHARACTERS FROM DAT
. COMPARE TO THE STRING POSS OF POSSIBLE OPTIONS
. IF ICH FOUND IN POSS SET THE CORRESPONDING FLAG IN VECTOR IOPT
UOPT60
          L,U       A5,1
UOPT62
          EX        UOPT80,*A3
          LXM,U     A2,UOPTB
          L         R1,R3             . N
          SE        A4,0,*A2
          J         RETUR4
          L         A1,R2             . LOC( IOPT(N-1) )
          AN        A1,R1
UOPT80
          S         A5,0,A1
          J         UOPT62
          ON    FOR
I DO 5 ,  L,14-I    A4,0,A0
          OFF . FOR
          ON    FTN
          L,Q1      A4,0,A0
          L,Q2      A4,0,A0
          L,Q3      A4,0,A0
          OFF . FTN
          J         $+1
 DO FOR , L,S6      A4,0,*A0
 DO FTN , L,Q4      A4,0,*A0
          LXM,U     A3,2
          J         UOPT62+1
#if (defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
.     DIAGNOSTIC PACKETS FOR FORT
#include "gerr_c.inc"
F         FORM      6,6,6,18
          INFO      010 03
$(3)
DIAG1     F         077,050,3,UOPT
          +         0,DYN$
          +         'UOPT  '
          ON        GERMAX
DIAG2     F         077,067,2,0
          +         LMJER1-FIRS01,DYN$
DIAG3     F         077,047,LDIAG3,LMJER1+1
          +         LDIAG3-2,DIAG2
 'UOPT( D,P,O,N ) ERROR:N.LE.0 OR N>30'
LDIAG3    EQU       $-DIAG3
          OFF
          ON        GERMAX=0
DIAG2     F         077,062,2,LMJER1+1
          +         LMJER1-FIRS01,DIAG1
          OFF
#endif
          END
#ifdef CERNLIB_TCGEN_UOPT
#undef CERNLIB_TCGEN_UOPT
#endif
