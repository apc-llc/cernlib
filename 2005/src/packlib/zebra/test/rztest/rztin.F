*
* $Id: rztin.F,v 1.1.1.1 1996/03/06 10:47:04 mclareni Exp $
*
* $Log: rztin.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:04  mclareni
* Zebra
*
*
#if defined(CERNLIB_INTER)
#include "test_include/pilot.h"
      SUBROUTINE RZTIN
*
********************************************************************************
*                                                                              *
*        To define menus and commands for the RZ interactive test program      *
*                                                                              *
*  Author  : R.Brun                                                            *
*  Written : 02.04.86                                                          *
*  Last mod: 02.05.86                                                          *
*                                                                              *
********************************************************************************
*
      CALL ZCMENU(1,'RZTEST$')
C
      CALL ZCBOOK( 1,'RZMAKE$',1)
      CALL ZCBOOK( 2,'RZFILE$',1)
      CALL ZCBOOK( 3,'NDIR$',1)
      CALL ZCBOOK( 4,'CDIR$',1)
      CALL ZCBOOK( 5,'MDIR$',1)
      CALL ZCBOOK( 6,'DELT$',1)
      CALL ZCBOOK( 7,'RZQUOT$',1)
      CALL ZCBOOK( 8,'LOCK$',1)
      CALL ZCBOOK( 9,'FREE$',1)
      CALL ZCBOOK(10,'LDIR$',1)
      CALL ZCBOOK(11,'PURGE$',1)
      CALL ZCBOOK(12,'DELK$',1)
      CALL ZCBOOK(13,'COPY$',1)
      CALL ZCBOOK(14,'CPT$',1)
      CALL ZCBOOK(15,'RZEND$' ,1)
      CALL ZCBOOK(16,'PATH$',1)
      CALL ZCBOOK(17,'RZLOGL$',1)
      CALL ZCBOOK(18,'RZOUT$',1)
      CALL ZCBOOK(19,'RZIN$',1)
      CALL ZCBOOK(20,'SAVE$',1)
      CALL ZCBOOK(21,'PQUEST$',1)
      CALL ZCBOOK(22,'PASSWORD$',1)
      CALL ZCBOOK(23,'WDIR$',1)
      CALL ZCBOOK(24,'STAT$',1)
      CALL ZCBOOK(25,'LLOK$',1)
      CALL ZCBOOK(26,'VOUT$',1)
      CALL ZCBOOK(27,'VIN$',1)
      CALL ZCBOOK(28,'VFILL$',1)
      CALL ZCBOOK(29,'PVECT$',1)
      CALL ZCBOOK(30,'TOFZ$',1)
      CALL ZCBOOK(31,'FRFZ$',1)
      CALL ZCBOOK(32,'FZOPEN$',1)
      CALL ZCBOOK(33,'FZEND$',1)
      CALL ZCBOOK(34,'RENK$',1)
C
      CALL ZCMENU(2,'MZCONTROL$')
C
      CALL ZCBOOK(1,'SURV$',2)
      CALL ZCBOOK(2,'SNAP$',2)
      CALL ZCBOOK(3,'VERI$',2)
      CALL ZCBOOK(4,'DZSHOW$',2)
      CALL ZCBOOK(5,'LINK$',2)
      CALL ZCBOOK(6,'STOR$',2)
      CALL ZCBOOK(7,'MZLOGL$',2)
      CALL ZCBOOK(8,'MZBOOK$',2)
      CALL ZCBOOK(9,'PZIN$',2)
      CALL ZCBOOK(10,'SDIVISION$',2)
      CALL ZCBOOK(11,'ZFREE$',2)
      CALL ZCBOOK(12,'ZUSED$',2)
      CALL ZCBOOK(13,'ZPURG$',2)
      CALL ZCBOOK(14,'ZNEWD$',2)
      CALL ZCBOOK(15,'BTREE$',2)
C
      END
#endif
