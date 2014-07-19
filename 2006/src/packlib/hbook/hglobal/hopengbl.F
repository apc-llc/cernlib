*
* $Id: hopengbl.F,v 1.1.1.1 1996/01/16 17:08:09 mclareni Exp $
*
* $Log: hopengbl.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#if defined(CERNLIB_VAX)
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.55  by  Rene Brun
*-- Author :
        FUNCTION HOPEN_GBL(FAB,RAB,IUNIT)
*
*       J.G. Loken      1/11/87
*
*       This function sets the UFO option, opens the file,
*       and returns the channel number and allocated space.
*
        COMMON/OPENCOM/INCHAN,APAGES
*
        INCLUDE '($FABDEF)/NOLIST'
        RECORD/FABDEF/FAB
*
        INTEGER*4 HOPEN_GBL,RET_STATUS,SYS$CREATE,APAGES
*
*********************************************************************
*
*       Set User File Open option.
*
        FAB.FAB$L_FOP=FAB.FAB$L_FOP.OR.FAB$M_UFO
*
*       Create/Open file, return channel number and allocated pages.
*
        RET_STATUS=SYS$CREATE(FAB,,)
        INCHAN=FAB.FAB$L_STV
        APAGES=FAB.FAB$L_ALQ
*
        HOPEN_GBL=RET_STATUS
        RETURN
        END
#endif
