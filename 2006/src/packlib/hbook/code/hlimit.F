*
* $Id: hlimit.F,v 1.3 2001/10/10 08:09:12 couet Exp $
*
* $Log: hlimit.F,v $
* Revision 1.3  2001/10/10 08:09:12  couet
* - Call MZFROM to initialise IOCC which is used later in HLABEL to create
*   alphamuneric labels data bank in histograms.
*
* Revision 1.2  1998/04/08 14:12:12  couet
* - VIDQQ was not used, and was initialised as a wrong "what" string.
*
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/14 04/10/94  12.53.47  by  Rene Brun
*-- Author :
      SUBROUTINE HLIMIT (LIMIT)
*.==========>
*.            Initialization routine for HBOOK
*.            IF(LIMIT>0) CALL MZEBRA and MZPAW
*.            IF(LIMIT=0) no calls to MZEBRA and MZPAW
*.            IF(LIMIT<0) CALL MZPAW
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcform.inc"
#include "hbook/hcdire.inc"
*.___________________________________________
*
      CALL HMACHI
*
      NHBOOK=IABS(LIMIT)
      IF(NHBOOK.LT.10000)NHBOOK=10000
      IF(LIMIT.GT.0)CALL MZEBRA(-3)
      IF(LIMIT.NE.0)CALL MZPAW(NHBOOK,' ')
      CALL MZLINK(IXPAWC,'/HCBOOK/',LHBOOK,LCDIR,LCIDN)
*
**************************************************************
*                                                            *
*--    Structural links in LHBOOK and LCDIR                  *
*                                                            *
*      lq(lcdir-1)= lsdir : pointer to subdirectory          *
*      lq(lcdir-2)= lids  : pointer to 1st ID in directory   *
*      lq(lcdir-3)= ltab  : pointer to list of ordered IDs   *
*      lq(lcdir-4)= lbuf  : pointer to ntuple buffers        *
*      lq(lcdir-5)= ltmp  : pointer to ntuple buffers        *
*      lq(lcdir-6)= lhquad: pointer to HQUAD buffers         *
*      lq(lcdir-7)=       : free                             *
*      lq(lcdir-8)=       : free                             *
*                                                            *
*--   Reference links in LHBOOK and LCDIR                    *
* R    lq(lcdir-9)= llid  : pointer to last ID in directory  *
*      lq(lcdir-10)=      : free                             *
*      lq(lcdir-11)=      : \                                *
*      ....               :  > free                          *
*      lq(lcdir-39)=      : /                                *
* R    lq(lcdir-40)=      : used by PAW                      *
* R    lq(lcdir-41)=      : used by PAW                      *
*      lq(lcdir-42)=      : \                                *
*      ....               :  > free                          *
*      lq(lcdir-50)=      : /                                *
**************************************************************
*
      IHWORK=IXPAWC+1
      IHDIV =IXPAWC+2
*
      CALL MZFORM('HDIR','4H -I',IODIR)
      CALL MZFORM('HID1','1B 2I 6F -H',IOH1)
      CALL MZFORM('HID2','1B 2I 3F 1I 4F -H',IOH2)
      CALL MZFORM('HIDN','11I -H',IOHN)
      CALL MZFORM('HIDT','13I -H',IONT)
      CALL MZFORM('HBLK','7I -H',IOBL)
      CALL MZFORM('HCF1','2I 2F 4D -F',IOCF1)
      CALL MZFORM('HCB1','2I 2F 4D -B',IOCB1)
      CALL MZFORM('HCF2','2I -F',IOCF2)
      CALL MZFORM('HCF4','4I -F',IOCF4)
      CALL MZFORM('HCB2','2I -B',IOCB2)
      CALL MZFORM('HFIT','5I 5F -D',IOFIT)
      CALL MZFORM('LCHX','2I -H',IOCC)
      CALL MZBOOK(IHDIV,LCDIR,LHBOOK, 1,'HDIR',50,8,10,IODIR,0)
      CALL UCTOH('PAWC            ',IQ(LCDIR+1),4,16)
      CALL MZBOOK(IHDIV,LTAB ,LHBOOK,-3,'HTAB',500,0,500,2,0)
*
      LMAIN =LHBOOK
      NLCDIR=1
      NLPAT =1
      CHCDIR(1)='PAWC'
      NCHTOP=1
      CHTOP(1)='PAWC'
      HFNAME(1)='COMMON /PAWC/ in memory'
      ICHTOP(1)=0
      ICHLUN(1)=0
      ICDIR=1
*
   10 RETURN
      END
