*
* $Id: fpcoms.inc,v 1.2 1996/03/01 16:49:50 cernlib Exp $
*
* $Log: fpcoms.inc,v $
* Revision 1.2  1996/03/01 16:49:50  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_FPCOMS_INC
#define CERNLIB_PAW_FPCOMS_INC
*
*
* fpcoms.inc
*
*CMZ :  2.07/00 24/04/95  17.58.46  by  O.Couet
*-- Author :    Gregory Kozlovsky   09/04/95
*
* tranfer data from pafith and pafitv to the fit panel
* and store data needed to draw graphics in HFGRUPDATE and VFGRUPDATE
* CHFOPTN - fit options, CHPOPTN - plot options
* HFXMIN,HFXMAX - the scale in x which does not change
*
      CHARACTER*32 CHFOPTN,CHPOPTN
      CHARACTER*80 XVECNAM,YVECNAM,EYVECNAM,
     +             VPARNAM,VLOWNAM,VUPPNAM,VSTEPNAM,VERRORNUM
      COMMON/FPCOM/  JAD,FPXMIN,FPXMAX,FPYMIN,FPYMAX
      COMMON/FPCOMC/ CHFOPTN,CHPOPTN,XVECNAM,YVECNAM,EYVECNAM,
     +               VPARNAM,VLOWNAM,VUPPNAM,VSTEPNAM,VERRORNUM

#endif
