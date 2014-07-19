*
* $Id: fasecm.inc,v 1.1.1.1 1996/03/07 15:17:55 mclareni Exp $
*
* $Log: fasecm.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:55  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FASECM_INC
#define CERNLIB_FATMEN_FASECM_INC
*
* For FMSEAC
*
* fasecm.inc
*
      COMMON/FASECM/LWRITE,NFILES,NDIRS,NMATCH,IOPT,
     +              LDSN,LHOST,LVID,LUSER,LFORM,LCOMM,LSTR,
     +              DSN,HOST,VID,USER,CHFILE,UFORM,COMM,CHSTR
      CHARACTER*255 CHFILE,CHSTR
      CHARACTER*255 HOST,VID,USER
      CHARACTER*255 DSN,UFORM,COMM
#include "fatmen/fatoptd.inc"

#endif
