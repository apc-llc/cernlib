*
* $Id: cdexam10.F,v 1.1.1.1 1996/02/28 16:23:55 mclareni Exp $
*
* $Log: cdexam10.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:55  mclareni
* Hepdb, cdlib, etc
*
*
      PROGRAM CDEXA10
*     ===============
*
*     Test Fortran CDUSE routine
*
      common/quest/iquest(100)
      PARAMETER     (NWPAW=100000)
      COMMON/PAWC/NWP,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(NWPAW)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      PARAMETER     (NKEYS=14)
      PARAMETER     (MAXOBJ=1000)
      PARAMETER     (NW=100)
      DIMENSION     KEYS(NKEYS),MASK(NKEYS)
      CHARACTER*10  CHFOR
      CHARACTER*4   CHTOP
      CHARACTER*80  CHFILE
      CHARACTER*8   CHTAG(NW)
 
      NWP = NWPAW
*
*     Initialise Zebra, HBOOK and HEPDB
*
      CALL CDPAW(NWPAW,NHBOOK,IDIV,'USR-DIV',5000,NWPAW,'ZPHU',IRC)
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
      CALL CDPREF(10,'CH',CHTOP,CHFILE,IRC)
*
*     Open the database file
*
      LRECL  = 0
      CALL CDOPEN(LUNCD,LUNFZ,CHTOP,CHFILE,LRECL,IDIV,' ',IRC)
*
*     Open another db
*
      CALL CDPREF(10,'C2',CHTOP,CHFILE,IRC)
      LRECL  = 0
      LUNCD1 = 3
      LUNFZ1 = 4
      CALL CDOPEN(LUNCD1,LUNFZ1,CHTOP,CHFILE,LRECL,IDIV,' ',IRC)
*
*     Set the log level
*
      CALL CDLOGL(' ',3,'A',IRC)
*
*     Get objects using full path
*
      NUMRUN = 1
      CALL CDUSE('//CDCH/GEOMETRY/CALOR/MASK',LBANK,NUMRUN,'ADF',IRC)
      PRINT *,NUMRUN,LBANK,IRC
 
      CALL MZDROP(IDIV,LBANK,' ')
      LBANK = 0
*
*     Get objects using alias
*
      CALL CDUSE('%3018',LBANK,NUMRUN,'ADF',IRC)
      PRINT *,NUMRUN,LBANK,IRC
      CALL MZDROP(IDIV,LBANK,' ')
      LBANK = 0
*
*     Get objects using alias
*
      CALL CDUSE('%3071',LBANK,NUMRUN,'ADF',IRC)
      PRINT *,NUMRUN,LBANK,IRC
      CALL CDUSE('%3018',LBANK,NUMRUN,'ADF',IRC)
      PRINT *,NUMRUN,LBANK,IRC
 
      CALL CDUSE('%3071',LBANK,NUMRUN,'ADF',IRC)
      PRINT *,NUMRUN,LBANK,IRC
 
      CALL CDUSE('//CDC2/5152',LBANK,NUMRUN,'ADF',IRC)
      PRINT *,NUMRUN,LBANK,IRC
*
*     Terminate
*
      CALL CDEND(' ','A',IRC)
 
      END
