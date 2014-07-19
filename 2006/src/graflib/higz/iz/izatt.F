*
* $Id: izatt.F,v 1.1.1.1 1996/02/14 13:11:08 mclareni Exp $
*
* $Log: izatt.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:08  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
*CMZ :  1.21/05 16/06/94  14.50.16  by  O.Couet
*-- Author :
      SUBROUTINE IZATT(IATTCO)
*.===========>
*.
*.   This routine stores in the current picture the REAL and INTEGER
*. HIGZ attributes. When an INTEGER attribute is stored in the data
*. structure, 100000 is added to the value of this attribute to
*. prevent the negatives values of some INTEGER attributes. In this
*. routine the case of the option MAIL is also managed.
*.   If CFLAG is true (default), attributes are stored only if they are
*. different from the current value in the bank. If CFLAG is false, they
*. are always stored. The value of CFLAG can be cahnged via a call to
*. IGZSET ('E' ==> false, 'C' ==> true).
*.
*. _Input parameters:
*.
*.  INTEGER IATTCO : Attribute code
*.
*..==========> (O.Couet)
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#include "higz/hiatnb.inc"
#include "higz/hiatt.inc"
#include "higz/hiflat.inc"
#include "higz/hiflag.inc"
#include "higz/hiaadr.inc"
      DIMENSION RATVAL(NBRATT)
      EQUIVALENCE (RATVAL(1),RMKSC)
      DIMENSION IATVAL(NBIATT)
      EQUIVALENCE (IATVAL(1),IPLCI)
      DIMENSION IRFLAT(NBRATT),IIFLAT(NBIATT)
      EQUIVALENCE (IRFLAT(1),KMKSC),(IIFLAT(1),KPLCI)
*.______________________________________
*
#if defined(CERNLIB_ZEBRA)
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IPOS=IATADR(IATTCO-49)
         IF(IPOS.LT.0)CALL IZSCAN(LHNT,IATTCO,ITBPTR-1,-1,IPOS)
         IATADR(IATTCO-49)=IPOS
*
         IF(IATTCO.LT.80)THEN
            INDEX=IATTCO-49
            IF(IRFLAT(INDEX).NE.0)THEN
               IF(IPOS.EQ.0)THEN
                  IF((Q(LHF+IRAPTR+INDEX-1).EQ.RATVAL(INDEX))
     +               .AND.CFLAG)RETURN
                  GOTO 10
               ELSE
                  IF((Q(LHF+IZGADR(LHNT,IPOS)).EQ.RATVAL(INDEX))
     +               .AND.CFLAG)RETURN
                  GOTO 10
               ENDIF
   10          IF(IZPUSH(0,1,0,'IZATT').NE.0)RETURN
               IATADR(IATTCO-49)=ITBPTR
               CALL IZSTCC(IATTCO,IFLPTR)
               Q(LHF+IFLPTR)=RATVAL(INDEX)
               CALL IZCFA(IATTCO,0)
               CALL IZINCF(1)
            ELSE
               IF(IPOS.EQ.0)THEN
                  CALL IZCDA(IATTCO)
               ELSE
                  Q(LHF+IZGADR(LHNT,IPOS))=RATVAL(INDEX)
               ENDIF
            ENDIF
         ELSE
            INDEX=IATTCO-79
            IF(IIFLAT(INDEX).NE.0)THEN
               IF(IPOS.EQ.0)THEN
                  IF((IQ(LHI+IIAPTR+INDEX-1).EQ.IATVAL(INDEX))
     +               .AND.CFLAG)RETURN
                  GOTO 20
               ELSE
                  IF((IZGADR(LHNT,IPOS).EQ.(IATVAL(INDEX)+100000))
     +               .AND.CFLAG)RETURN
                  GOTO 20
               ENDIF
   20          IF(IZPUSH(0,0,0,'IZATT').NE.0)RETURN
               IATADR(IATTCO-49)=ITBPTR
               CALL IZSTCC(IATTCO,IATVAL(INDEX)+100000)
               CALL IZCFA(IATTCO,0)
            ELSE
               IF(IPOS.EQ.0)THEN
                  CALL IZCDA(IATTCO)
               ELSE
                  IQ(LHNT+IPOS)=1000000*IATTCO+(IATVAL(INDEX)+100000)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         IF(IATTCO.LT.80)THEN
            IND=IATTCO-49
            WRITE (CHMAIL,'(I3,E16.7)') IATTCO,RATVAL(IND)
         ELSE
            IND=IATTCO-79
            WRITE (CHMAIL,'(I3,E16.7)') IATTCO,FLOAT(IATVAL(IND))
         ENDIF
         CALL IMWRIT(4)
      ENDIF
#endif
*
      END
#endif
