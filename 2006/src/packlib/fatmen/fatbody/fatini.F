*
* $Id: fatini.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: fatini.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FATINI(ISTOR,LUNRZ,LUNFZ,CHTOP,CHOPT)
*
************************************************************************
*
*     SUBROUTINE FATINI(ISTOR,LUNRZ,LUNFZ,CHTOP,CHOPT)
*
*
*     initialize the FATMEN - user interface package -
*
*     ISTOR   User store index
*
*     LUNRZ   unit for the RZ database
*     LUNFZ   unit for the FZ files
*
*     CHOPT    Character Option (to be defined)
*
************************************************************************
*
      PARAMETER       (MNSYS = 10000, MXSYS = 100000)
*
*             ZEBRA system common blocks
*
#include "zebra/mzcb.inc"
      COMMON /ZEBQ/   IQFENC(4), LQZEB(100)
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatlnk.inc"
*
      CHARACTER CHTOP*(*)
      CHARACTER*(*)  CHOPT
      PARAMETER       (LKEYFA=10)
      CHARACTER*8 CHTAG(LKEYFA)
      CHARACTER*10 CHFOR
      COMMON /PAWC/ NWPAW,IXPAWC,IHBOOK,IXHIGZ,IXKUIP,IFENCE(5)
      DATA CHTAG/'Num.Id.',5*'Fname'
     +          ,'cp.level','loc.code','medium','nm.banks'/
      DATA CHFOR/'IHHHHHIIII'/
*_______________________________________________________________________
*
*
*
**      initialize some variables
*
        LPRTFA = 6
        LUFZFA = LUNFZ
*
*  **   DB System initialization
*
        IF(ISTOR.GE.0) THEN
        CALL MZDIV  (ISTOR, IDIVFA, 'FAT-SYSTM', MNSYS, MXSYS, 'P')
        LQST   = LOCF(LQ(1)) - 1
        LQZB   = LQST + 1 - LOCF(LQZEB(1))
        CALL MZSDIV (0, -1)
        KOFSFA = KQS - LQZB
        CALL MZSDIV (IDIVFA, -1)
        KOFUFA = KQS - LQZB
        CALL MZLINK (IDIVFA, '/FATUSE/', LBFXFA, LBDSFA, LBFXFA)
        CALL MZLINK (     0, '/FATSTO/', LDIRFA, LBOUT , LJNKFA)
        CALL MZLINK (IDIVFA, '/FATSER/', LNEWFA, LBANKR, LNEWFA)
        CALL MZLINK (IDIVFA, '/FATBNK/', LBKLUN(1), LBKLUN(99),
     +               LBKLUN(1))
        CALL MZLINK (IDIVFA, '/FATLNK/', LOPNBK, LCP2BK, LOPNBK)
 
        CALL MZFORM ('UPFA', '10I -H', IOUPFA)
        CALL MZFORM ('BKFA', '-I', IOBKFA)
        CALL MZFORM ('DSFA', '66H 3I 11H 8I 1H 2I 2H 3I 8H 21I 20H'
     +               ,IODSFA)
        NTOPFA = 0
        ENDIF
#if !defined(CERNLIB_CZ)
*
**      Initialize the RZ - db
*
#endif
      NRECS = 65000
      IF(IDEBFA.GE.2) PRINT *,'FATINI. call FARINI for ',
     +   LUNRZ,CHTOP,LKEYFA,CHFOR,CHTAG,NRECS,CHOPT
       CALL FARINI (LUNRZ,CHTOP,LKEYFA,CHFOR,CHTAG,NRECS,CHOPT)
 
*                                                             END FATINI
      END
 
 
