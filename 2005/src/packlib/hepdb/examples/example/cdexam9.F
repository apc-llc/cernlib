*
* $Id: cdexam9.F,v 1.2 1996/04/16 06:46:48 jamie Exp $
*
* $Log: cdexam9.F,v $
* Revision 1.2  1996/04/16 06:46:48  jamie
* Correct size of /PAWC/
*
* Revision 1.1.1.1  1996/02/28 16:23:55  mclareni
* Hepdb, cdlib, etc
*
*
      PROGRAM CDEXA9
*     ==============
*
*     Test Fortran CDUSEM routine
*
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
      CALL CDPREF(10,'AU',CHTOP,CHFILE,IRC)
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
*     Get objects using selection mask
*
      NUMRUN = 1
      CALL VZERO(MASK,NKEYS)
      MASK(10) = 1
      KEYS(10) = 4271809
      CALL CDUSEM('//CDAU/AUX/HWP3',LBANK,NUMRUN,MASK,KEYS,' ',IRC)
      PRINT *,'Return code ',IRC,' from CDUSEM'
*
*     Terminate
*
      CALL CDEND(' ','A',IRC)
 
      END
