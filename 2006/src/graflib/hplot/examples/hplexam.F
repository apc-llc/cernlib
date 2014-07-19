*
* $Id: hplexam.F,v 1.3 1998/01/30 15:24:59 couet Exp $
*
* $Log: hplexam.F,v $
* Revision 1.3  1998/01/30 15:24:59  couet
* - APOLLO version removed
*
* Revision 1.2  1997/03/14 14:24:06  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:25:41  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.13/00 01/07/92  16.13.03  by  O.Couet
*-- Author :    O.Couet   30/05/91
      PROGRAM HPLEXA
*.==========>
*.           HPLOT TEST PROGRAM
*..=========>
      CHARACTER*(*) HZFILE,HPFILE
#if defined(CERNLIB_IBM)
      PARAMETER (HZFILE='/HPLOT HIGZ')
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_PSCRIPT))
      PARAMETER (HPFILE='/HPLOT METAFILE')
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_PSCRIPT))
      PARAMETER (HPFILE='/HPLOT PS')
#endif
#if !defined(CERNLIB_IBM)
      PARAMETER (HZFILE='hplot.higz')
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_PSCRIPT))
      PARAMETER (HPFILE='hplot.metafile')
#endif
#if (!defined(CERNLIB_IBM))&&(defined(CERNLIB_PSCRIPT))
      PARAMETER (HPFILE='hplot.ps')
#endif
      COMMON/PAWC/H(100000)
      LOGICAL INTRAC
*.___________________________________________
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_X11))
      CALL INITC
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_IBM)||defined(CERNLIB_CRAY)
      OPEN(UNIT= 1,FILE=HZFILE,FORM='UNFORMATTED',RECL=4096,
     +     ACCESS='DIRECT',STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_VAX)
      OPEN(UNIT=1,FILE=HZFILE,FORM='UNFORMATTED',RECL=1024,
     +     ACCESS='DIRECT',SHARED,STATUS='UNKNOWN')
#endif
#if !defined(CERNLIB_VAX)
      OPEN(UNIT=10,FILE=HPFILE,FORM='FORMATTED',STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_VAX)
      OPEN(UNIT=10,FILE=HPFILE,FORM='FORMATTED',SHARED,
     +     STATUS='UNKNOWN')
#endif
      IF(.NOT.INTRAC())THEN
         KWTYPE=0
      ELSE
         CALL IGWKTY(KWTYPE)
      ENDIF
      CALL TIMED(T0)
      CALL HLIMIT(100000)
      CALL HPLINT(KWTYPE)
      CALL HPLMAK
      IF(KWTYPE.NE.0)THEN
         CALL HPLOPT('PTO ',1)
         CALL HPLEX1
         CALL TIMED(T1)
         PRINT *, ' TIME FOR EXAMPLE 1 =',T1,'  SECONDS'
         CALL HPLEX2
         CALL TIMED(T2)
         PRINT *, ' TIME FOR EXAMPLE 2 =',T2,'  SECONDS'
         CALL HPLEX3
         CALL TIMED(T3)
         PRINT *, ' TIME FOR EXAMPLE 3 =',T3,'  SECONDS'
         CALL HPLEX4
         CALL TIMED(T4)
         PRINT *, ' TIME FOR EXAMPLE 4 =',T4,'  SECONDS'
         CALL HPLEX5
         CALL TIMED(T5)
         PRINT *, ' TIME FOR EXAMPLE 5 =',T5,'  SECONDS'
      ENDIF
      CALL HPLOPT('NPTO',1)
*
*          Open HIGZ metafile
*          and repeat previous examples
*
      PRINT *,' WRITING HIGZ PICTURE FILE'
      CALL IGZSET('Z')
      CALL IZFILE(1,'HPLOT','NA')
      CALL HPLOPT('ZFL ',1)
      CALL HPLEX6
      CALL TIMED(T6)
      PRINT *, ' TIME TO WRITE HIGZ PICTURE FILE =',T6,'  SECONDS'
*
*          Open a GKS or PostScript metafile
*          and repeat previous examples
*
      PRINT *,' WRITING METAFILE (BE PATIENT !)'
      CALL IGZSET('G')
      CALL HPLOPT('NZFL',1)
      CALL HPLCAP(-10)
      CALL HPLEX6
      CALL TIMED(T7)
      PRINT *, ' TIME TO WRITE METAFILE =',T7,'  SECONDS'
*
*          Replay some pictures from the HIGZ picture file
*
      IF(KWTYPE.NE.0)THEN
         CALL HPLCAP(0)
         CALL HPLEX7
      ENDIF
*
      CALL HPLEND
      END
