*
* $Id: datimh.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: datimh.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
. **************************************
.
.     SUBROUTINE DATIMH( ND, NT )                *** Z007 ***
#if defined(CERNLIB_GUYFORE)||defined(CERNLIB_GUYFORT)
.     RETURNS FIELDATA  DATE  AND  TIME
#endif
#if defined(CERNLIB_GUYFTN)
.     RETURNS ASCII DATE AND TIME
#endif
.     ND = 8HDD/MM/YY,    NT = 8HHH.MM.SS
.
. **************************************
 
          AXR$
          FORT$
 
#if defined(CERNLIB_GUYFORE)||defined(CERNLIB_GUYFORT)
$(0)
PERIOD    +         0750000750000
SLASH     +         0007400007400
 
#include "gerr_c.inc"
 
$(1)
DATIMH*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          ER        DATE$
          L         A5,BLANK
          S         A0,A4
          S         A5,A2
          DSL       A4,12    .                   --------MMDDYYBBBB
          DSL       A1,12    . --------HHMMSSBBBB
          SSC       A4,12    .                   ------DD--MMYYBBBB
          LDSL      A0,24    . ----HHMM----SSBBBB
          LDSL      A3,12    .                   ----DD--MM--YYBBBB
          SSC       A1,6     . ----HH-MM---SSBBBB
          A         A4,SLASH .                   ----DD-/MM/-YYBBBB
          A         A1,PERIOD .    HH.MM.--SSBBBB
          LSSC      A4,6     .                   ----DD/MM/--YYBBBB
          DSC       A0,12    . ------HH.MM.SSBBBB
          DSC       A3,12    .                   ------DD/MM/YYBBBB
          L         A0,0,X11           . LOC(DATE)
          L         A3,1,X11           . LOC(TIME)
          DS        A4,0,A0            . DATE
          DS        A1,0,A3            . TIME
          J         RETUR2
#endif
#if defined(CERNLIB_GUYFTN)
SLATE     INFO      2 2
$(2),WORD RES  2
$(1)
DATIMH*
          DL        A2,0,XARG
          ER        DATE$
          DS        A0,WORD
          L,U       A1,056000
          L,S3      A4,WORD
          L,S1      A0,WORD+1
          L,S2      A5,WORD
          LXI,S4    A1,WORD+1
          SSC       A4,27
          SSC       A0,27
          SSC       A5,18
          A,S4      A4,WORD
          A,S2      A0,WORD+1
          A,U       A5,057000
          A,S5      A1,WORD+1
          SSC       A4,18
          SSC       A0,18
          A,S5      A5,WORD
          SSC       A1,27
          A,U       A4,057000
          A,U       A0,056000
          SSC       A5,27
          A,S6      A1,WORD+1
          A,S1      A4,WORD
          A,S3      A0,WORD+1
          A,S6      A5,WORD
          DS        A0,0,A3            . TIME
          DS        A4,0,A2            . DATE
          J         RETUR2
#endif
          END
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
