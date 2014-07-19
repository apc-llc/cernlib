*
* $Id: hmapg.F,v 1.1.1.1 1996/01/16 17:08:09 mclareni Exp $
*
* $Log: hmapg.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#if defined(CERNLIB_VAX)
#include "hbook/pilot.h"
*CMZ :  4.10/05 29/03/89  14.37.04  by  Rene Brun
*-- Author :
      FUNCTION hmapg(global_name,base_common,off_set)
*
*       J.G. Loken      1/11/87
*
*       Function to dynamically map to an existing global section.
*
*       This function maps to the global section using SYS$MGBLSC,
*       allocating pages in the p0 region with the sec$m_expreg option.
*
*       GLOBAL_NAME is the name of the section to be mapped.
*       BASE_COMMON is the first word of reference COMMON.
*       OFF_SET is the offset from BASE_COMMON of the mapped section in words.
*                       i.e., BASE_COMMON(OFF_SET) is the first word.
*       HMAPG is returned equal to the global section length (pages) if OK,
*               or as an error if not (< 0).
*
      CHARACTER*(*) global_name
      CHARACTER*80 name
      INTEGER*4 base_common,off_set,new_addr,old_addr
      INTEGER*4 inad(2),rtad(2),flag
      INTEGER*4 hmapg
*
      INTEGER*4 sys$mgblsc
*
      INCLUDE '($SECDEF)/NOLIST'
*
***************************************************************************
*
      CALL str$trim(name,global_name,length)
      inad(1)=0
      inad(2)=0
*
*       Give read/write access to mapped section.
*       Also, use the expreg option to get its own VM space.
*
      flag=sec$m_gbl+sec$m_wrt+sec$m_expreg
*
      hmapg=sys$mgblsc(inad,rtad,,%val(flag),name(1:length),,)
      IF((hmapg.AND.1).EQ.0) THEN
        hmapg=-hmapg
        RETURN
      ELSE
        hmapg=(rtad(2)-rtad(1)+1)/512
      ENDIF
*
      new_addr=rtad(1)
      old_addr=%loc(base_common)
      off_set=1+(new_addr-old_addr)/4
*
      RETURN
      END
 
#endif
