*
* $Id: kcesvr.inc,v 1.1.1.1 1996/03/08 15:32:59 mclareni Exp $
*
* $Log: kcesvr.inc,v $
* Revision 1.1.1.1  1996/03/08 15:32:59  mclareni
* Kuip
*
*
#ifndef CERNLIB_KUIP_KCESVR_INC
#define CERNLIB_KUIP_KCESVR_INC
*
*
* kcesvr.inc
*
*-- when changing MAXSVR ajust also the size in /CKUIP/KUGSVR
      PARAMETER      (MAXSVR =20)
      INTEGER         NSVFIL, NSVCUR, IESADD
      LOGICAL         SERVER
      COMMON /KCESVR/ NSVFIL, NSVCUR, IESADD, SERVER

      CHARACTER*32    EDTFIL(MAXSVR)
      CHARACTER*32    EDTCMD(MAXSVR)
      COMMON /KCEFIL/ EDTFIL, EDTCMD


#endif
