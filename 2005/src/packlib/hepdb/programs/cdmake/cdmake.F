*
* $Id: cdmake.F,v 1.2 1996/03/12 13:07:35 cernlib Exp $
*
* $Log: cdmake.F,v $
* Revision 1.2  1996/03/12 13:07:35  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:34  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CDMAKE
*     ==============
*
*     Create a new, empty database
*
      PARAMETER   (NWPAW=100000)
      COMMON/PAWC/PAW(NWPAW)
#include "hepdb/slate.inc"
      CHARACTER*255 CHLINE
      CHARACTER*255 CHFILE
*
*     Initialise Zebra, HBOOK and HEPDB
*
      CALL CDPAW(NWPAW,NHBOOK,IDIV,'USR-DIV',5000,50000,'ZPHU',IRC)
*
*     Unit for database access
*
      LUNCD  = 1
*
*     Get database parameters
*
      CALL GETENVF('CDFILE',CHFILE)
      IF(IS(1).EQ.0) THEN
         CHFILE = 'HEPDB.DBS'
         LFILE  =  9
         WRITE(6,9001) CHFILE(1:LFILE)
 9001 FORMAT(' CDMAKE. database name defaulted to ',A,/,
     +       ' (environmental variable CDFILE not set)')
      ELSE
         LFILE = IS(1)
         WRITE(6,9002) CHFILE(1:LFILE)
 9002 FORMAT(' CDMAKE. database name set to ',A)
      ENDIF
 
      CALL GETENVF('CDPAIR',CHLINE)
      IF(IS(1).EQ.0) THEN
         NPAIR  = 1
         WRITE(6,9003) NPAIR
 9003 FORMAT(' CDMAKE. # of validity pairs defaulted to ',I3,/,
     +       ' (environmental variable CDPAIR not set)')
      ELSE
         NPAIR = ICDECI(CHLINE,1,IS(1))
         IF(NPAIR.LE.0) THEN
            WRITE(6,9004) NPAIR
 9004 FORMAT(' CDMAKE. # of validity pairs invalid: ',I3)
            STOP 4
         ENDIF
         WRITE(6,9005) NPAIR
 9005 FORMAT(' CDMAKE. # of validity pairs set to ',I3)
      ENDIF
 
      CALL GETENVF('CDPRE',CHLINE)
      IF(IS(1).EQ.0) THEN
         NPRE   = 10000
         WRITE(6,9006) NPRE
 9006 FORMAT(' CDMAKE. # of preformatted records defaulted to ',I6,/,
     +       ' (environmental variable CDPRE not set)')
      ELSE
         NPRE = ICDECI(CHLINE,1,IS(1))
         WRITE(6,9007) NPRE
 9007 FORMAT(' CDMAKE. # of preformatted records set to ',I3)
      ENDIF
 
      CALL GETENVF('CDTOP',CHLINE)
      IF(IS(1).EQ.0) THEN
         NTOP   = 1
         WRITE(6,9008) NTOP
 9008 FORMAT(' CDMAKE. database numeric ID defaulted to ',I3,/,
     +       ' (environmental variable CDTOP not set)')
      ELSE
         NTOP  = ICDECI(CHLINE,1,IS(1))
         WRITE(6,9009) NTOP
 9009 FORMAT(' CDMAKE. database numeric ID set to ',I3)
      ENDIF
 
      CALL GETENVF('CDQUO',CHLINE)
      IF(IS(1).EQ.0) THEN
         NQUO   = 65000
         WRITE(6,9010) NQUO
 9010 FORMAT(' CDMAKE. database quota defaulted to ',I6,/,
     +       ' (environmental variable CDQUO not set)')
      ELSE
         NQUO  = ICDECI(CHLINE,1,IS(1))
         WRITE(6,9011) NQUO
 9011 FORMAT(' CDMAKE. database quota defaulted to ',I3)
      ENDIF
 
      CALL GETENVF('CDRECL',CHLINE)
      IF(IS(1).EQ.0) THEN
         LRECL  = 1024
         WRITE(6,9012) LRECL
 9012 FORMAT(' CDMAKE. record length defaulted to ',I6,' words ',/,
     +       ' (environmental variable CDRECL not set)')
      ELSE
         LRECL = ICDECI(CHLINE,1,IS(1))
         WRITE(6,9013) LRECL
 9013 FORMAT(' CDMAKE. record length set to ',I6,' words ')
      ENDIF
*
      CALL CDNEW(LUNCD,'HEPDB',CHFILE(1:LFILE),IDIV,
     +           NPAIR,NQUO,NPRE,NTOP,LRECL,'F',IRC)
*
*     Set the log level
*
      CALL CDLOGL(' ',3,'A',IRC)
*
*     Terminate
*
      CALL CDEND(' ','A',IRC)
 
      END
