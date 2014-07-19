*
* $Id: datime.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: datime.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
. **************************************
.
.     SUBROUTINE DATIME( ID, IT )                *** Z007 ***
.     RETURNS  INTEGER  DATE  AND  TIME
.     ID = YYMMDD,    IT = HHMM
.
. **************************************
 
          AXR$
          FORT$
SLATE     INFO  2  2
$(2),WORD RES  1
 
$(1)
DATIME*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
 DO FTN , S         XARG,A3
          ER        TDATE$
          S         A0,WORD
          SZ        A1
          L,S3      A4,WORD
          L,H2      A2,WORD
          A,U       A4,64
          DI,U      A1,60
          MSI,U     A4,100
          SZ        A0
          A,S1      A4,WORD
          DI,U      A0,60
 DO FOR , DL        A2,0,XARG          . LOC(ID), LOC(IT)
 DO FTN , DL        A2,0,A3
          MSI,U     A4,100
          MSI,U     A0,100
          A,S2      A4,WORD
          A         A0,A1
          S         A4,0,A2            . ID
          S         A0,0,A3            . IT
          J         RETUR2
          END
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
