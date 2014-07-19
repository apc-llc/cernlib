*
* $Id: cdexam3.F,v 1.1.1.1 1996/02/28 16:23:54 mclareni Exp $
*
* $Log: cdexam3.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:54  mclareni
* Hepdb, cdlib, etc
*
*
      PROGRAM CDEXA3
*     ==============
*
*     Test alias manipulation
*
      PARAMETER     (NWPAW=100000)
      COMMON/PAWC/  PAW(NWPAW)
      PARAMETER     (NKEYS=10)
      PARAMETER     (MAXOBJ=1000)
      CHARACTER*8   CHTAG(NKEYS)
      CHARACTER*10  CHFOR
      CHARACTER*4   CHTOP
      CHARACTER*80  CHFILE
      CHARACTER*255 CHALIA
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
*     Print the current alias definitions
*
      CALL CDALIA('//CDAA/DIRA/DIRB/DIRC/DIRD',' ','P',IRC)
      PRINT *,'CDEXAM3. return code ',IRC,' from CDALIA'
*
*     Enter a new alias for this session
*
      CALL CDALIA('//CDAA/DIRA/DIRB/DIRC/DIRD','FRODO','S',IRC)
      PRINT *,'CDEXAM3. return code ',IRC,' from CDALIA'
*
*     Print the equivalence name for FRODO
*
      CALL CDALIA(CHALIA,'FRODO','R',IRC)
      PRINT *,'CDEXA3. equivalence name for FRODO is ',
     +         CHALIA(1:LENOCC(CHALIA))
*
*     Get the current alias definitions
*
      CHALIA = ' '
      CALL CDALIA('//CDAA/DIRA/DIRB/DIRC/DIRD',CHALIA,'G',IRC)
      PRINT *,'CDEXAM3. return code ',IRC,' from CDALIA'
      PRINT *,'CDEXA3. alias definition is ',CHALIA(1:LENOCC(CHALIA))
*
*     Enter a different alias into the database
*
      CALL CDALIA('//CDAA/DIRA/DIRB/DIRC/DIRD','BILBO','U',IRC)
      PRINT *,'CDEXAM3. return code ',IRC,' from CDALIA'
*
*     Terminate
*
      CALL CDEND(' ','A',IRC)
 
      END
