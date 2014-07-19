*
* $Id: cduscm.inc,v 1.1.1.1 1996/02/28 16:24:04 mclareni Exp $
*
* $Log: cduscm.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:04  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CDUSCM_INC
#define CERNLIB_HEPDB_CDUSCM_INC
*
*
* cduscm.inc
*
      COMMON /CDUSCM/ IDEBCD, IDISCD, IDIVCD, IHKYCD, IKDRCD
     +              , KOFSCD, KOFUCD
     +              , LTOPCD, LBADCD, LBAFCD, LBDACD,    LBFXCD, LBFYCD
     +              , LBKYCD, LBNOCD, LFIXCD, LREFCD(7), LSAVCD, LBUPCD
     +              , LPRTCD, NTOPCD
      PARAMETER       (NDMXCB=25000)
*

#endif
