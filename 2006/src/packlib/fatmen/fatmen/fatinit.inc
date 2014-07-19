*
* $Id: fatinit.inc,v 1.1.1.1 1996/03/07 15:17:54 mclareni Exp $
*
* $Log: fatinit.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:54  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATINIT_INC
#define CERNLIB_FATMEN_FATINIT_INC
*
*
* fatinit.inc
*
      IF(SERNAM(1:4).EQ.'INIT') THEN
         PRINT *,'Error - FATMEN has not been initialised'
         RETURN
      ENDIF

#endif
