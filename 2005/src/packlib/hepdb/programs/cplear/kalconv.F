*
* $Id: kalconv.F,v 1.1.1.1 1996/02/28 16:23:36 mclareni Exp $
*
* $Log: kalconv.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:36  mclareni
* Hepdb, cdlib, etc
*
*
#include "sys/CERNLIB_machine.h"
#include "_hepdb/pilot.h"
      PROGRAM KALCONV
*
*     Program to convert CPLEAR calibration database
*     to HEPDB format
*
*     RZKAL keys: VAL_STAR (I)
*                 VAL_STOP (I)
*                 DETECTOR (H)
*                 BANK ID  (H)
*     insertion time = RZ date/time
*
*     HEPDB keys: NPAIR    = 1
*                 VAL_STAR = KEYS(11) (I)
*                 VAL_STOP = KEYS(12) (I)
*                 NUSER    = 2
*                 DETECTOR = KEYS(13) (H)
*                 BANK ID  = LEYS(14) (H)
*     insertion time = KEYS(IDHINS)
*
*     Output pathnames:
*
*     //CDCD/CALIBRATION/DC_LT
*     //CDCD/CALIBRATION/DC_MT
*     //CDCD/CALIBRATION/DC_ST
*
*     //CDCD/CALIBRATION/PC_ST
*     //CDCD/CALIBRATION/PC_LT
*     //CDCD/CALIBRATION/PC_MT
*
*     //CDCD/CALIBRATION/PID_ST
*     //CDCD/CALIBRATION/PID_LT
*     //CDCD/CALIBRATION/PID_MT
*
*     //CDCD/CALIBRATION/ST_MT
*
*     //CDCD/CALIBRATION/CALO_LT
*
      PARAMETER    (NWPAW=100000)
      COMMON/PAWC/ PAW(NWPAW)
      COMMON/USRLNK/IDIV,LADDR
      CHARACTER*4  CHTOP
      CHARACTER*80 CHFILE
      EXTERNAL     CPKALC
*
*     Initialise Zebra, HBOOK and HEPDB
*
      CALL CDPAW(NWPAW,NHBOOK,IDIV,'USR-DIV',5000,50000,'ZPHU',IRC)
*
*     Link area of banks retrieved from database
*
      CALL MZLINK(IDIV,'/USRLNK/',LADDR,LADDR,LADDR)
*
*     Unit for database access
*
      LUNCD  = 1
*
*     Unit for database update (via journal files)
*
      LUNFZ  = 2
*
*     Unit for RZKAL file
*
      LUNRZ  = 3
*
*     Open CPLEAR calibration database (RZKAL.DATA)
*
      LRECL  = 0
      CALL RZOPEN(LUNRZ,'RZKAL','rzkal.data',' ',LRECL,IRC)
      CALL RZFILE(LUNRZ,'RZKAL',' ')
*
*     Find the database file and construct the top directory name
*
      CALL CDPREF(10,'CD',CHTOP,CHFILE,IRC)
*
*     Open the database file
*
      LRECL  = 0
      CALL CDOPEN(LUNCD,LUNFZ,CHTOP,CHFILE,LRECL,IDIV,' ',IRC)
*
*     Loop over directories in RZKAL.DATA
*
      CALL RZSCAN('//RZKAL',CPKALC)
*
*     Terminate
*
      CALL CDEND(' ','A',IRC)
      CALL RZCLOS(' ','A')
 
      END
