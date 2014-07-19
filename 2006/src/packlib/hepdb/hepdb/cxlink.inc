*
* $Id: cxlink.inc,v 1.1.1.1 1996/02/28 16:24:06 mclareni Exp $
*
* $Log: cxlink.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:06  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CXLINK_INC
#define CERNLIB_HEPDB_CXLINK_INC
*
*
* cxlink.inc
*
      PARAMETER       (MAXJCX=20, MAXVCX= 20, LUKYCX=88, LUDACX=89)
      COMMON /CXLINK/ ISTOCX, KEYSCX(100), MASKCX(100), L3PRCX, LURZCX
     +              , LFRSCX, LJOICX, LKJNCX(MAXJCX), LDJNCX(MAXJCX)
     +              , LVIWCX, LKVWCX(MAXVCX), LDVWCX(MAXVCX), LASTCX
*

#endif
