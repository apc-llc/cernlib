*
* $Id: cdexam6.F,v 1.1.1.1 1996/02/28 16:23:55 mclareni Exp $
*
* $Log: cdexam6.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:55  mclareni
* Hepdb, cdlib, etc
*
*
      PROGRAM CDEXA6
*     ==============
*
*     Test Zebra structure manipulation
*
      PARAMETER     (NWPAW=100000)
      COMMON/PAWC/NWP,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(NWPAW)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      PARAMETER     (NKEYS=10)
      PARAMETER     (MAXOBJ=1000)
      PARAMETER     (NW=100)
      DIMENSION     KEYS(NKEYS)
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
*     Store a bank
*
      DO 10 I=1,NKEYS
         KEYS(I) = I*10
10    CONTINUE
      JBIAS = 2
      CALL MZBOOK(IDIV,LADDR,LSUP,JBIAS,'TEST',0,0,50,2,0)
 
      DO 20 I=1,50
         IQ(LADDR+I) = 51 - I
20    CONTINUE
      CALL CDSTOR('//CDAA/DIRA/DIRB/DIRC/DIRD',LADDR,LKYBK,IDIV,
     +            KEYS,'A',IRC)
*
*     Terminate
*
      CALL CDEND(' ','A',IRC)
 
      END
