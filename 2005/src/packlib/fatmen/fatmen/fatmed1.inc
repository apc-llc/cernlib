*
* $Id: fatmed1.inc,v 1.1.1.1 1996/03/07 15:17:57 mclareni Exp $
*
* $Log: fatmed1.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:57  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATMED1_INC
#define CERNLIB_FATMEN_FATMED1_INC
*
*
* fatmed1.inc
*
*     SYSREQ TMS Q DEVTYPES
*     TMS Model
      DATA          MFMTYP(1)/'DISK'/,MFMTYP(2)/'3480'/,
     +              MFMTYP(3)/'3420'/,MFMTYP(4)/'8200'/,
     +              MFMTYP(5)/'8500'/,MFMTYP(6)/'3490'/,
     +              MFMTYP(7)/'DAT60'/,MFMTYP(8)/'DAT90'/,
     +              MFMTYP(9)/'3490E'/,MFMTYP(10)/'AMPEX'/,
     +              MFMTYP(11)/'REDWOOD'/,MFMTYP(12)/'DLT2'/,
     +              MFMTYP(13)/'DAT120'/,MFMTYP(14)/'3590'/
 
*     TMS Sort
      DATA          MFMGEN(1)/'DISK'/,MFMGEN(2)/'CT1 '/,
     +              MFMGEN(3)/'TAPE'/,MFMGEN(4)/'8MM '/,
     +              MFMGEN(5)/'8MM '/,MFMGEN(6)/'CT2 '/,
     +              MFMGEN(7)/'DAT '/,MFMGEN(8)/'DAT '/,
     +              MFMGEN(9)/'CT2E'/,MFMGEN(10)/'DD2 '/,
     +              MFMGEN(11)/'DD3 '/,MFMGEN(12)/'DLT '/,
     +              MFMGEN(13)/'DAT '/,MFMGEN(14)/'NTP '/
 
*     TMS density code
      DATA          MFMDEN(1)/'N/A '/,MFMDEN(2)/'38K '/,
     +              MFMDEN(3)/'6250'/,MFMDEN(4)/'43200'/
     +              MFMDEN(5)/'86400'/,MFMDEN(6)/'38KDC'/,
     +              MFMDEN(7)/'DDSC'/,MFMDEN(8)/'DDSC'/,
     +              MFMDEN(9)/'....'/,MFMDEN(10)/'....'/,
     +              MFMDEN(11)/'....'/,MFMDEN(12)/'10GC'/,
     +              MFMDEN(13)/'DDSC'/,MFMDEN(14)/'10GC'/
 
*     Maximum capacity
      DATA          MFMSIZ(1)/'N/A'/, MFMSIZ(2)/'200'/,
     +              MFMSIZ(3)/'200'/, MFMSIZ(4)/'2300'/,
     +              MFMSIZ(5)/'5000'/,MFMSIZ(6)/'1000'/,
     +              MFMSIZ(7)/'2000'/,MFMSIZ(8)/'5000'/,
     +              MFMSIZ(9)/'2000'/,MFMSIZ(10)/'25000'/,
     +              MFMSIZ(11)/'40000'/,MFMSIZ(12)/'10000'/,
     +              MFMSIZ(13)/'4000'/,MFMSIZ(14)/'10000'/
 
*     Default mount type
      DATA          MFMMNT(1)/'M'/,   MFMMNT(2)/'M'/,
     +              MFMMNT(3)/'M'/,   MFMMNT(4)/'M'/,
     +              MFMMNT(5)/'M'/,   MFMMNT(6)/'M'/,
     +              MFMMNT(7)/'M'/,   MFMMNT(8)/'M'/,
     +              MFMMNT(9)/'M'/,   MFMMNT(10)/'M'/,
     +              MFMMNT(11)/'M'/,  MFMMNT(12)/'M'/,
     +              MFMMNT(13)/'M'/,  MFMMNT(14)/'M'/
 
*     Default label type
      DATA          MFMLAB(1)/'  '/,  MFMLAB(2)/'SL'/,
     +              MFMLAB(3)/'SL'/,  MFMLAB(4)/'SL'/,
     +              MFMLAB(5)/'SL'/,  MFMLAB(6)/'SL'/,
     +              MFMLAB(7)/'SL'/,  MFMLAB(8)/'SL'/,
     +              MFMLAB(9)/'SL'/,  MFMLAB(10)/'SL'/,
     +              MFMLAB(11)/'SL'/, MFMLAB(12)/'SL'/,
     +              MFMLAB(13)/'SL'/, MFMLAB(14)/'SL'/
*
*     Number of media types defined
*
      DATA          NFTYPS/14/
*
*     TMS density codes
*
*     Model  Density  Code     P
*     ------ -------- -------- -
*     3420   1600         1600 Y
*     3420   6250         6250 Y
*     3480   38KC           15 Y
*     3480   38KD           20 Y
*     3480   38K         38000 N
*     3480   38000       38000 Y
*     3490   38KCD          10 N
*     3490   38KDC          10 Y
*     8200   8200         8200 Y
*     8200   8500         8500 Y

#endif
