*
* $Id: fatsys.inc,v 1.1.1.1 1996/03/07 15:17:58 mclareni Exp $
*
* $Log: fatsys.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:58  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATSYS_INC
#define CERNLIB_FATMEN_FATSYS_INC
*
*
* fatsys.inc
*
*
*     Common to store name of service machine (IBM)
*     or default directory (VAX, Unix)
*
      COMMON/FATSYS/STRMLF,IFMODX,SERNAM,SERMOD,FATNOD,DEFAULT,
     +              FATTOP,TOPDIR,LOCALQ
*     Mode at which server disk is accessed (VM)
      CHARACTER*1   SERMOD
*     Name of service machine
      CHARACTER*20  SERNAM
      CHARACTER*64  FATTOP
      CHARACTER*16  TOPDIR
      CHARACTER*80  FATNOD
      CHARACTER*132 LOCALQ
      CHARACTER*132 DEFAULT
      LOGICAL       STRMLF

#endif
