*
* $Id: cdexam2.F,v 1.1.1.1 1996/02/28 16:23:54 mclareni Exp $
*
* $Log: cdexam2.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:54  mclareni
* Hepdb, cdlib, etc
*
*
      PROGRAM CDEXA2
*     ==============
*
*     Modify an existing database
*
      PARAMETER    (NWPAW=100000)
      COMMON/PAWC/ PAW(NWPAW)
*
*     Number of user keys: system, experiment and # pairs were
*     defined at database creation time
*
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
         PRINT *,'CDEXAM2. stopping due to fatal error from CDPREF'
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
*     Create some directories
*
      CHFOR = 'IIIIIIIIII'
      DO 10 I=1,NKEYS
         WRITE(CHTAG(I),'(A6,I2.2)') 'USRKEY',I
10    CONTINUE
      DELTA = 0.0
      IPREC = 0
      CALL CDMDIR('//CDAA/DIRA/DIRB/DIRC/DIRD',NKEYS,CHFOR,CHTAG,MAXOBJ,
     +            IPREC,DELTA,'CP',IRC)
*
*     Terminate
*
      CALL CDEND(' ','A',IRC)
 
      END
