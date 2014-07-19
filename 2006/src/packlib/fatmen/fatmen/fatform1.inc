*
* $Id: fatform1.inc,v 1.2 1996/09/23 14:23:58 jamie Exp $
*
* $Log: fatform1.inc,v $
* Revision 1.2  1996/09/23 14:23:58  jamie
* Add EBCD (EBCDIC) format
*
* Revision 1.1.1.1  1996/03/07 15:17:57  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATFORM1_INC
#define CERNLIB_FATMEN_FATFORM1_INC
*
*
* fatform1.inc
*
      DATA FATFRM(1)/'FZ  '/,
     +     FATFRM(2)/'FA  ' /,
     +     FATFRM(3)/'FX  ' /,
     +     FATFRM(4)/'FFX ' /,
     +     FATFRM(5)/'RZ  ' /,
     +     FATFRM(6)/'RX  ' /,
     +     FATFRM(7)/'EP  ' /,
     +     FATFRM(8)/'AS  ' /,
     +     FATFRM(9)/'UN  ' /,
     +     FATFRM(10)/'DA  ' /,
     +     FATFRM(11)/'FXN ' /,
     +     FATFRM(12)/'FPT ' /,
     +     FATFRM(13)/'FPS ' /,
     +     FATFRM(14)/'FPD ' /,
     +     FATFRM(15)/'FPK ' /,
     +     FATFRM(16)/'FPO ' /,
     +     FATFRM(17)/'YBB ' /,
     +     FATFRM(18)/'YBD ' /,
     +     FATFRM(19)/'RWN ' /,
     +     FATFRM(20)/'CWN ' /,
     +     FATFRM(21)/'EBCD' /
 
      DATA FRCFM(1)/'F   '/,
     +     FRCFM(2)/'FB  '/,
     +     FRCFM(3)/'V   '/,
     +     FRCFM(4)/'VB  '/,
     +     FRCFM(5)/'VBS '/,
     +     FRCFM(6)/'VS  '/,
     +     FRCFM(7)/'U   '/,
     +     FRCFM(8)/'RMS '/
 

#endif
