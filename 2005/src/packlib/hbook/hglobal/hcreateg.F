*
* $Id: hcreateg.F,v 1.1.1.1 1996/01/16 17:08:09 mclareni Exp $
*
* $Log: hcreateg.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#if defined(CERNLIB_VAX)
#include "hbook/pilot.h"
*CMZ :  4.22/09 21/07/94  18.55.55  by  Fons Rademakers
*-- Author :
      FUNCTION hcreateg(global_name,base_common,size)
*
*       J.G. Loken      1/11/87
*
*       Last modification: 16/11/93 - cater for variable size
*                                     pages on AXP (JDS)
*
*       Function to create and map a global section.
*
*       This function first opens a file with UFO option (using HST_OPEN_GBL),
*       then creates and maps the global section using SYS$CRMPSC.
*       Open the file using SYS$SETDFPROT to set protection loose.
*
*       GLOBAL_NAME is the name of the section to be mapped.
*       BASE_COMMON is the first word of the COMMON to be mapped.
*       SIZE is the size of the common in words.
*       HCREATEG is returned equal to the global section length (pages) if OK,
*               or as an error if not (< 0).
*
*       HCREATEG$DIR is a logical which, if defined, gives the directory
*               for the mapping file of the global section.
*               In this case, the file is not deleted upon closing.
*
      INCLUDE '($SECDEF)/NOLIST'
      INCLUDE '($SSDEF)/NOLIST'
*     INCLUDE '($SYIDEF)/NOLIST'
*
*     Hard code item code for pre V5.5 systems
*
      PARAMETER (SYI$_PAGE_SIZE=4452)
*
      CHARACTER*(*) global_name
      CHARACTER*255 ERRMSG
      CHARACTER*80 name
      CHARACTER*80 file_dir
      CHARACTER*7  open_status
      INTEGER*4 len_dir,istat,lib$sys_trnlog,length
      INTEGER*4 base_common,size
      INTEGER*4 inad(2),rtad(2),flag,inchan,npages,apages
      INTEGER*4 hcreateg,iprot,jprot
      INTEGER*4 str$trim,sys$getsyiw
*
      INTEGER*4 hopen_gbl
      EXTERNAL hopen_gbl
      COMMON/opencom/inchan,apages
      INTEGER*4 gbl_lun
*
*       System services
*
      INTEGER*4 sys$crmpsc,sys$getsyi,sys$setdfprot
      INTEGER*4 sys$getmsg
      INTEGER*4 sec$m_temp/0/    !test replacement for perm
 
      STRUCTURE /ITMLST/
         UNION
            MAP
            INTEGER*2 BUFFER_LENGTH
              INTEGER*2 ITEM_CODE
            INTEGER*4 BUFFER_ADDRESS
              INTEGER*4 RETURN_LENGTH_ADDRESS
            ENDMAP
            MAP
            INTEGER*4 END_LIST /0/
            ENDMAP
         END UNION
      END STRUCTURE
 
      RECORD /ITMLST/ SYI_LIST(2)
 
      INTEGER IOSB(2)
*
*
***************************************************************************
*
      istat = str$trim(name,global_name,length)
      npages=(size+127)/128
*
*       Look for directory for mapping file.
*
*
      istat = lib$sys_trnlog ('HCREATEG$DIR',len_dir,file_dir)
      IF (istat .EQ. SS$_NORMAL) THEN
        open_status = 'UNKNOWN'
      ELSE
        file_dir = ' '
        len_dir = 0
        open_status = 'SCRATCH'
      ENDIF
*
*       Setup histogram global section.
*
      istat = lib$get_lun(gbl_lun)
      iprot = 0
      istat = sys$setdfprot(iprot,jprot)
*
      OPEN(UNIT=gbl_lun,STATUS=open_status,
     &  FILE=file_dir(1:len_dir)//name(1:length)//'.GBL',
     &  RECL=512,RECORDTYPE='FIXED',NOSPANBLOCKS,INITIALSIZE=NPAGES,
     &  EXTENDSIZE=512,CARRIAGECONTROL='NONE',USEROPEN=hopen_gbl)
*
      istat = sys$setdfprot(jprot,iprot)
*
      npages=min(npages,apages)
      inad(1)=%loc(base_common)
      if ( mod(inad(1),512) .ne. 0) then
        call hbug('Start address of global section '''//name(1:length)//
     +            ''' is not PAGE aligned','HCREATEG',0)
      endif
*
*     Get page size of this machine
*
      SYI_LIST(1).BUFFER_LENGTH         = 4
      SYI_LIST(1).ITEM_CODE             = SYI$_PAGE_SIZE
      SYI_LIST(1).BUFFER_ADDRESS        = %LOC(ISIZE)
      SYI_LIST(1).RETURN_LENGTH_ADDRESS = %LOC(LSIZE)
 
      SYI_LIST(2).END_LIST              = 0
 
      ISTAT = SYS$GETSYIW(,,,SYI_LIST,IOSB,,)
*
*     Assume that we are on a VAX in case of problems
*     (e.g. unknown item code)
*
      IF(.NOT.ISTAT.OR..NOT.IOSB(1)) ISIZE = 512
 
      inad(2)=inad(1)+ISIZE*npages-1
      flag=sec$m_gbl+sec$m_wrt+sec$m_temp
      hcreateg=sys$crmpsc(inad,rtad,,%val(flag)
     &          ,name(1:length),,,%VAL(inchan),%VAL(npages),,,)
      IF((hcreateg.AND.1).EQ.0) THEN
*
*     Get error message
*
        istat   = sys$getmsg(%val(hcreateg),lenmsg,errmsg,,)
        CALL HBUG(ERRMSG(1:LENMSG),'HCREATEG',0)
        hcreateg=-hcreateg
      ELSE
        hcreateg=(rtad(2)-rtad(1)+1)/ISIZE
      ENDIF
*
      RETURN
      END
#endif
