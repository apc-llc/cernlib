*
* $Id: cdexam4.F,v 1.1.1.1 1996/02/28 16:23:55 mclareni Exp $
*
* $Log: cdexam4.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:55  mclareni
* Hepdb, cdlib, etc
*
*
      PROGRAM CDEXA3
*     ==============
*
*     Test mnemonic name manipulation
*
      PARAMETER     (NWPAW=100000)
      COMMON/PAWC/  PAW(NWPAW)
      PARAMETER     (NKEYS=10)
      PARAMETER     (MAXOBJ=1000)
      PARAMETER     (NW=100)
      CHARACTER*10  CHFOR
      CHARACTER*4   CHTOP
      CHARACTER*80  CHFILE
      CHARACTER*8   CHTAG(NW)
      call errset(0)
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
*     Get current definitions (if any)
*
      print *,'get current mnemonic name definitions'
      CALL CDNAME('//CDAA/DIRA/DIRB/DIRC/DIRD',NW,CHTAG,'R',IRC)
      print *,(chtag(i),i=1,nw)
*
*     Store some definitions - these are somewhat arbitrary
*
      CHTAG(1) = 'X'
      CHTAG(2) = 'Y'
      CHTAG(3) = 'Z'
      CHTAG(4) = 'PX'
      CHTAG(5) = 'PY'
      CHTAG(6) = 'PZ'
      CHTAG(7) = 'E'
      CHTAG(8) = 'DX'
      CHTAG(9) = 'DY'
      CHTAG(10)= 'DZ'
      print *,'store some mnemonic name definitions'
      CALL CDNAME('//CDAA/DIRA/DIRB/DIRC/DIRD',10,CHTAG,'U',IRC)
*
*     Now print current definitions
*
      print *,'print the mnemonic name definitions'
      CALL CDNAME('//CDAA/DIRA/DIRB/DIRC/DIRD',10,CHTAG,'P',IRC)
*
*     and delete them
*
*     print *,'delete the mnemonic name definitions'
*     CALL CDNAME('//CDAA/DIRA/DIRB/DIRC/DIRD',10,CHTAG,'D',IRC)
*
*     Terminate
*
      CALL CDEND(' ','A',IRC)
 
      END
