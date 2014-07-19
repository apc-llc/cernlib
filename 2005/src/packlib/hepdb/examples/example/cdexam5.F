*
* $Id: cdexam5.F,v 1.1.1.1 1996/02/28 16:23:55 mclareni Exp $
*
* $Log: cdexam5.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:55  mclareni
* Hepdb, cdlib, etc
*
*
      PROGRAM CDEXA5
*     ==============
*
*     Modify an existing database
*
      PARAMETER    (NWPAW=100000)
      COMMON/PAWC/ PAW(NWPAW)
      PARAMETER    (NKEYS=10)
      PARAMETER    (MAXOBJ=1000)
      CHARACTER*8  CHTAG(NKEYS)
      CHARACTER*10 CHFOR
      CHARACTER*4  CHTOP
      CHARACTER*80 CHFILE
*
*     Initialise Zebra, HBOOK and HEPDB
*
      CALL CDPAW(NWPAW,NHBOOK,IDIV,'USR-DIV',5000,50000,'ZPHU',IRC)
*
*     Unit for database access
*
      LUNCD  = 1
*
*     Unit for database update (via journal files)
*
      LUNFZ  = 2
*
*     Find the database file and construct the top directory name
*
      CALL CDPREF(10,'AA',CHTOP,CHFILE,IRC)
      IF(IRC.NE.0) THEN
         PRINT *,'CDEXAM5. stopping due to fatal error from CDPREF'
         STOP 16
      ENDIF
*
*     Open the database file
*
      LRECL  = 0
      CALL CDOPEN(LUNCD,LUNFZ,CHTOP,CHFILE,LRECL,IDIV,' ',IRC)
*
*     Set the log level
*
      CALL CDLOGL(' ',3,'A',IRC)
*
*     Delete some directories
*
      CALL CDDDIR('//CDAA/DIRA/DIRB/DIRC/DIRD',' ',IRC)
*
*     Terminate
*
      CALL CDEND(' ','A',IRC)
 
      END
