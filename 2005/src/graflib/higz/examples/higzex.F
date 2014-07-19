*
* $Id: higzex.F,v 1.2 1997/03/14 14:26:04 mclareni Exp $
*
* $Log: higzex.F,v $
* Revision 1.2  1997/03/14 14:26:04  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:25:16  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/02/14 13:10:20  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.15/00 10/07/92  09.47.37  by  O.Couet
*-- Author :
      PROGRAM HIGZEX
*.==========>
*.
*.           HIGZ TEST PROGRAM
*.
*..=========>
      COMMON/PAWC/H(20000)
      LOGICAL INTRAC
      CHARACTER*80 STR
      CHARACTER*(*) HZFILE
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_PSCRIPT))
      PARAMETER (HZFILE='/HIGZ METAFILE')
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_PSCRIPT))
      PARAMETER (HZFILE='/HIGZ PS')
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_PSCRIPT))
      PARAMETER (HZFILE='higz.metafile')
#endif
#if (!defined(CERNLIB_IBM))&&(defined(CERNLIB_PSCRIPT))
      PARAMETER (HZFILE='higz.ps')
#endif

*.___________________________________________
*
#if defined(CERNLIB_IBM)
      CALL ERRSET(151,999,-1)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_X11))
      CALL INITC()
#endif
      OPEN(10,FILE=HZFILE,FORM='FORMATTED',STATUS='UNKNOWN')
      CALL MZEBRA(-3)
      CALL MZPAW(20000,' ')
      CALL IGINIT(0)
      IF(.NOT.INTRAC())THEN
         INTER=0
         KWTYPE=0
      ELSE
         CALL IGWKTY(KWTYPE)
         INTER=1
      ENDIF
      CALL IGSSE(6,KWTYPE)
      IF(INTER.EQ.0)GOTO 10
      CALL HIEX1
      CALL IRQST(1,1,ISTAT,NCH,STR)
*
*          Switch to alpha mode. Note that IGSSE has preset the
*          workstation identifier to 1
*
      CALL IGSA (1)
*
      PRINT *, ' Example 1 completed'
      CALL HIEX2
      CALL IRQST(1,1,ISTAT,NCH,STR)
      CALL IGSA (1)
      PRINT *, ' Example 2 completed'
*
      CALL HIEX3
      CALL IRQST(1,1,ISTAT,NCH,STR)
      CALL IGSA (1)
      PRINT *, ' Example 3 completed'
*
      CALL HIEX4
      CALL IRQST(1,1,ISTAT,NCH,STR)
      CALL IGSA (1)
      PRINT *, ' Example 4 completed'
*
  10  CALL HIEX5
      IF(INTER.EQ.0)GOTO 20
      CALL IGSA (1)
      PRINT *, ' Example 5 completed'
*
*          Replay some pictures from the HIGZ metafile
*
      CALL HIEX6
      CALL IGSA (1)
      PRINT *, ' Example 6 completed'
*
  20  CALL IGEND
      END
