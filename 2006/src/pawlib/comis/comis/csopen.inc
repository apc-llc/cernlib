*
* $Id: csopen.inc,v 1.1.1.1 1996/02/26 17:16:35 mclareni Exp $
*
* $Log: csopen.inc,v $
* Revision 1.1.1.1  1996/02/26 17:16:35  mclareni
* Comis
*
*
#ifndef CERNLIB_COMIS_CSOPEN_INC
#define CERNLIB_COMIS_CSOPEN_INC
*
*
* csopen.inc
*
      CHARACTER FILEOP*64,STATOP*8,ACCEOP*12,FORMOP*12
      COMMON/CSOPEC/FILEOP,STATOP,ACCEOP,FORMOP
      COMMON/CSOPEI/LUNOP,LRECOP

#endif
