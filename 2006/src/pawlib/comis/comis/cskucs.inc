*
* $Id: cskucs.inc,v 1.1.1.1 1996/02/26 17:16:35 mclareni Exp $
*
* $Log: cskucs.inc,v $
* Revision 1.1.1.1  1996/02/26 17:16:35  mclareni
* Comis
*
*
#ifndef CERNLIB_COMIS_CSKUCS_INC
#define CERNLIB_COMIS_CSKUCS_INC
*
*
* cskucs.inc
*
      PARAMETER ( KUVTOT=500, KUVOFS=15)
      CHARACTER *32 KUVNMS
      COMMON/CSKUNM/KUVNMS(KUVTOT)
      COMMON/CSKULK/JKUVBS,KUVCUR,KUVLNK(KUVTOT)

#endif
