C-                                                        -
C-  INPUT:                                                -
C-  PRUNIT=  print unit                                   -
C-                                                        -
C-         SDP Jan.,1985, Jan.,1987                       -
C-   SK  Apr.,1986,   Add calling for ISV2,ISP2 and ISP3  -
C-                                                        -
C----------------------------------------------------------
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/izisac.inc"
#include "isajet/izisaj.inc"
#include "isajet/izisal.inc"
#include "isajet/izisaq.inc"
#include "isajet/izisv1.inc"
#include "isajet/izisp1.inc"
#include "isajet/zebcom.inc"
      INTEGER PRUNIT,EVTR
      INTEGER LISAJ,LISAQ,LISV1,LISP1,LISAC,LISCL,LISAL
      INTEGER LISV2,LISP2,LISP3,LPJET,GZPJET
C
      IF(LISAE.LE.0) RETURN
        CALL UCTOH('ISAE',EVTR,4,4)
        IF(IQ(LISAE-4).NE.EVTR) RETURN  ! not event record
        CALL PRISAE(PRUNIT,LISAE,0,0,0)
C
      LISAJ=LQ(LISAE-IZISAJ)
      IF(LISAJ.GT.0)
     $  CALL PRISAJ(PRUNIT,LISAJ,0,'LINEAR',0)    ! print jet banks
C
      LISAQ=LQ(LISAE-IZISAQ)
      IF(LISAQ.GT.0)
     $   CALL PRISAQ(PRUNIT,LISAQ,0,'LINEAR',0)    ! print parton banks
C
      IF(IQ(LISAE-2).GT.7) THEN ! check ISAE has link for PJET
        LPJET=GZPJET()
        IF(LPJET.NE.0)
     &    CALL PRPJET(PRUNIT,0,0,'ALL',0)
      ENDIF
C
      LISV1=LQ(LISAE-IZISV1)
   3  IF(LISV1.GT.0) THEN          ! vertex banks
        CALL PRISV1(PRUNIT,LISV1,0,'ONE',0)
        LISP1=LQ(LISV1-IZISP1)
C
        IF(LISP1.GT.0)             ! particle banks
     $      CALL PRISP1(PRUNIT,LISP1,0,'LINEAR',0)
C
        LISV1=LQ(LISV1)
        GOTO 3
      ENDIF
C
      LISV2=LQ(LISAE-4)
   4  IF(LISV2.GT.0) THEN
        CALL PRISV2(PRUNIT,LISV2,0,'ONE',0)
        LISP2=LQ(LISV2-1)
C
        IF(LISP2.GT.0)
     $     CALL PRISP2(PRUNIT,LISP2,0,'LINEAR',0)
C
        LISV2=LQ(LISV2)
        GO TO 4
      ENDIF
C
      LISP3=LQ(LISAE-5)
      IF(LISP3.GT.0)
     $    CALL PRISP3(PRUNIT,LISP3,0,'LINEAR',0)
C
      LISAC=LQ(LISAE-IZISAC)
      IF(LISAC.NE.0) THEN
        CALL PRISAC(PRUNIT,LISAC,0,'ONE',0) ! calorimeter description
        CALL PRISCL(PRUNIT,0,0,'ALL',0)     ! print all cells
      ENDIF
      LISAL=LQ(LISAE-IZISAL)
      IF(LISAL.GT.0) CALL PRISAL(PRUNIT,LISAL,0,'LINEAR',0) ! leptons
      CALL PRISJT(PRUNIT,0,0,'ALL',0)
      RETURN
      END
