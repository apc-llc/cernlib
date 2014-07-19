*
* $Id: dzdfzs.F,v 1.1.1.1 1996/03/04 16:13:06 mclareni Exp $
*
* $Log: dzdfzs.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:06  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDFZS
*      IMPLICIT NONE
      INTEGER NCH,   INFLUN,INFSTA, INFOFZ(40), LUN,
     &        MEDIU, IFIFO, IDAFO, IACMO, IRWMO
      INTEGER JBYT
      COMMON/FZSTAT/ INFLUN,INFSTA, INFOFZ
      CHARACTER*4 CHLUN
      CHARACTER*8 MEDTYP(0:6),FIFTYP(0:4), DAFTYP(0:1),ACMTYP(0:3),
     &            RWMODE(0:3)
      DATA MEDTYP/'disk','tape', 'K disk','usertape','C disk',
     &            'tape, ch','memory'/
      DATA FIFTYP/'native','Xchange','Direct a','memory','Alpha'/
      DATA DAFTYP/'native','exchange'/
      DATA ACMTYP/'Fortran','Yspecial','L C-lib','C chann'/
      DATA RWMODE/'  ','R ','W ','RW'/
 
      CALL KUGETC(CHLUN,NCH)
      IF(NCH.LE.0)GOTO 999
      CALL DZDCTI(CHLUN,LUN)
      CALL FZINFO(LUN)
      IF(INFLUN.NE.LUN)THEN
         WRITE(*,*)'Error from FZINFO'
         GOTO 999
      ENDIF
      WRITE(*,*)'***** FzInfo for file open on unit: ',LUN,' *****'
      MEDIU=JBYT(INFSTA,1,3)
      WRITE(*,*)'Medium       : ',MEDTYP(MEDIU)
      IFIFO=JBYT(INFSTA,4,3)
      WRITE(*,*)'File Format  : ',FIFTYP(IFIFO)
      IDAFO=JBYT(INFSTA,7,1)
      WRITE(*,*)'Data Format  : ',DAFTYP(IDAFO)
      IACMO=JBYT(INFSTA,8,3)
      WRITE(*,*)'Access mode  : ',ACMTYP(IACMO)
      IRWMO=JBYT(INFSTA,11,2)
      WRITE(*,*)'Open for     : ',RWMODE(IRWMO)
      WRITE(*,*)'Log level    : ',INFOFZ(4)
      WRITE(*,*)'Words read/wr: ',INFOFZ(19)*10000000+ INFOFZ(20)
999   END
***********************************************************************
