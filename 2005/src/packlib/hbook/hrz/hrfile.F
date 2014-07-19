*
* $Id: hrfile.F,v 1.2 2001/12/12 16:21:52 couet Exp $
*
* $Log: hrfile.F,v $
* Revision 1.2  2001/12/12 16:21:52  couet
* - New option E. Used with the option Q, this option gives the possibility to
*   create HBOOK files with up to 2**32 records. This uses of the option N
*   of RZMAKE. This option (E) is also available via HROPEN. For example:
*
*       IQUEST(10)=256 000
*       CALL HROPEN(1,'EXAMPLE','example.rz','NQE',LREC,ISTAT)
*
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/13 22/09/94  16.21.15  by  Rene Brun
*-- Author :
      SUBROUTINE HRFILE(LUN,CHDIR,CHOPT)
*.==========>
*.           OPEN a new HBOOK/RZ file if CHOPT='N'
*.           OPEN an existing HBOOK/RZ file if CHOPT=' '
*.           If option 'G' assumes Global section being defined
*.              Global section start at address LUN
*.           If option 'M' assumes Global memory being defined
*.              Global memory start at address LUN
*.           If option 'Q' the allocation for the file is
*.              given by user in IQUEST(10)
*.           If option 'QE' the allocation for the file is
*.              given by user in IQUEST(10) but in an Extended way ie:
*.              the file can have up to 2**32 records instead of 2**16
*.              with the option Q
*.           If option 'C' the record length is given via IQUEST(99)
*..=========> ( R.Brun )
#include "hbook/hcdire.inc"
#include "hbook/hcmail.inc"
      COMMON/QUEST/IQUEST(100)
      CHARACTER*(*) CHDIR,CHOPT
      CHARACTER*8   TAGS(2),CHOPTT
      DIMENSION IOPT(6)
      EQUIVALENCE (IOPTN,IOPT(1)),(IOPTG,IOPT(2)),(IOPTQ,IOPT(3))
      EQUIVALENCE (IOPTM,IOPT(4)),(IOPTO,IOPT(5)),(IOPTE,IOPT(6))
*.___________________________________________
*
      IF(NCHTOP.GE.MXFILES)THEN
         CALL HBUG('Too many open files','HRFILE',LUN)
         GO TO 99
      ENDIF
*
      CALL HUOPTC(CHOPT,'NGQMOE',IOPT)
      IF(IOPTM.NE.0)IOPTG=1
*
      IQUEST(1)=0
      IF(IOPTG.EQ.0)THEN
         IF(IOPTN.NE.0)THEN
            IF(IOPTQ.NE.0)THEN
               NQUOT=IQUEST(10)
               IF(NQUOT.LT.100)NQUOT=100
               IF(NQUOT.GT.65000.AND.IOPTE.EQ.0)NQUOT=65000
            ELSE
               NQUOT=32000
            ENDIF
            TAGS(1) = 'HBOOK-ID'
            TAGS(2) = 'VARIABLE'
            NCH=LENOCC(CHOPT)
            IF(NCH.EQ.0)THEN
               CHOPTT='X'
            ELSE
               CHOPTT='X'//CHOPT(1:NCH)
            ENDIF
            CALL CLTOU(CHOPTT)
*-* option 'N' is a new option in ZEBRA (new format with 7 words/key)
            I=INDEX(CHOPTT,'N')
            IF(I.NE.0)CHOPTT(I:I)='?'
*-* option 'E' correspond to option 'N' in RZMAKE (to have 2**32 records)
            I=INDEX(CHOPTT,'E')
            IF(I.NE.0)CHOPTT(I:I)='N'
            IF(IOPTO.NE.0)THEN
               NWK=1
               CHOPTT(1:1)='?'
            ELSE
               NWK=2
            ENDIF
*-- what a mess using IQUEST(10) for two different things
*-- the guy who invented that should be punished (rdm)
            IQ10=IQUEST(10)
            IF(INDEX(CHOPT,'C').NE.0) IQUEST(10)=IQUEST(99)
            CALL RZMAKE(LUN,CHDIR,NWK,'II',TAGS,NQUOT,CHOPTT)
            IQUEST(10)=IQ10
         ELSE
            IQ10=IQUEST(10)
            IF(INDEX(CHOPT,'C').NE.0) IQUEST(10)=IQUEST(99)
            CALL RZFILE(LUN,CHDIR,CHOPT)
            IQUEST(10)=IQ10
            IF(IQUEST(1).EQ.2)IQUEST(1)=0
            NWK=IQUEST(8)
         ENDIF
      ENDIF
      IF(IQUEST(1).NE.0)RETURN
*
      NCHTOP=NCHTOP+1
      CHTOP(NCHTOP)=CHDIR
      ICHLUN(NCHTOP)=0
      IF(IOPTG.EQ.0)THEN
         ICHTOP(NCHTOP)=LUN
         ICHTYP(NCHTOP)=NWK
         HFNAME(NCHTOP)=CHDIR
      ELSE
         ICHTOP(NCHTOP)=-LOCF(LUN)
         ICHTYP(NCHTOP)=0
         IF(IOPTM.EQ.0)THEN
            HFNAME(NCHTOP)='Global section - '//CHDIR
         ELSE
            HFNAME(NCHTOP)='Global memory  - '//CHDIR
         ENDIF
      ENDIF
*
  10  CHMAIL='//'//CHTOP(NCHTOP)
      CALL HCDIR(CHMAIL,' ')
*
  99  RETURN
      END
