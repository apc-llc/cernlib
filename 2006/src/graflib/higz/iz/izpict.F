*
* $Id: izpict.F,v 1.1.1.1 1996/02/14 13:11:12 mclareni Exp $
*
* $Log: izpict.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:12  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.19/16 22/10/93  09.33.30  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IZPICT(PNAMEI,CHOPT)
*.===========>
*.
*.    This routine acts on the picture representation in the ZEBRA data
*. base according to the values of CHOPT. The following diagram shows
*. the general structure of an HIGZ picture.
*.
*.
*.              General structure of an HIGZ picture
*.             ______________________________________
*.
*.                      LPICT
*.                        |
*.                        |
*.  +----+----+----+----+-V-+----------------------------------\
*.  | -4 | -3 | -2 | -1 |   | Top control bank (TCB)            >
*.  +--+-+--+-+--+-+--+-+---+----------------------------------/
*.     |LHC |LHF |LHI |LHNT0
*.     |    |    |    |         +----------\
*.     |    |    |    +-------->| NT banks  >
*.     |    |    |              +----------/
*.     |    |    |              +--------------+
*.     |    |    +------------->| Integer bank |
*.     |    |                   +--------------+
*.     |    |                   +-----------+
*.     |    +------------------>| Real bank |
*.     |                        +-----------+
*.     |                        +----------------+
*.     +----------------------->| Character bank |
*.                              +----------------+
*.
*.                      Content of the Top Control bank
*.                     _________________________________
*.
*.        1                2                 3                 4
*. +--------------+-----------------+-----------------+----------------+
*. | PNAME length | Integer bank    | Real bank       | Character bank |
*. | in character | pointer: INTPTR | pointer: IFLPTR | pointer: ISTPTR|
*. +--------------+-----------------+-----------------+----------------+
*.
*.     5        6        7        8
*. +--------+--------+--------+--------+
*. | IRISIZ | IRFSIZ | IRSSIZ | INBNT  |
*. +--------+--------+--------+--------+
*.
*.                      Content of a NT bank
*.                     ______________________
*.
*.        1                  2                        3            4
*. +-----------+----------------------------+------------------+-------+
*. | NT Number | Window and Viewport adress | Display Priority | ITBPTR|
*. +-----------+----------------------------+------------------+-------+
*.
*.     5                 6                       7
*. +-------+--------------------------+-----------------------+
*. | Level | Integer Attributes Flags | Real Attributes Flags |
*. +-------+--------------------------+-----------------------+
*.
*.                     8
*. +------------------------------------------+
*. | Adress of the default Integer attributes |
*. +------------------------------------------+
*.
*.                     9
*. +---------------------------------------+
*. | Adress of the default real attributes |
*. +---------------------------------------+
*.
*.     10 ...
*. +---------- - - - -  -   -    -     -
*. | Graphics primitives and attributes
*. +---------- - - - -  -   -    -     -
*.
*.                      Content of the Integer bank
*.                     _____________________________
*.
*.          1 .... NBIATT
*. +------------------------------------------+
*. |  Integer attributes word of the NT 0     |
*. +------------------------------------------+
*.
*.       NBIATT ...
*. +----------- - - - -  -   -
*. | Graphics Integer data
*. +----------- - - - -  -   -
*.
*.                      Content of the Real bank
*.                     __________________________
*.
*.          1 .... NBRATT
*. +---------------------------------------+
*. |  Real attributes word of the NT 0     |
*. +---------------------------------------+
*.
*.       NBRATT ...
*. +----------- - - - -  -   -
*. | Graphics Real data
*. +----------- - - - -  -   -
*.
*.                      Content of the Character bank
*.                     _______________________________
*.
*.      1 .... 5
*. +-----------------+
*. |  Picture name   |
*. +-----------------+
*.
*.       6 ...
*. +----------- - - - -  -   -
*. | Character data
*. +----------- - - - -  -   -
*.
*. _Input parameters:
*.
*.  CHARACTER PNAMEI : Picture name . Input parameter with the options :
*.                     M,D,S,P,C,O
*.  CHARACTER CHOPT : Option .
*.
*.              CHOPT='M'ake a new picture.
*.              CHOPT='D'isplay the picture PNAMEI.
*.              CHOPT='S'cratch picture.
*.              CHOPT='N'ext picture.
*.              CHOPT='L'ist pictures.
*.              CHOPT='AL' Full listing of the pictures.
*.              CHOPT='F'irst picture.
*.              CHOPT='P'rint the contain of a picture.
*.              CHOPT='C'urrent picture.
*.              CHOPT='R'etrieve picture name.
*.              CHOPT='Q'uiet (not error message).
*.              CHOPT='G' retrieve the dispalyed (Graphic) picture name.
*.              CHOPT='O'rder the NT banks in the picture PNAMEI
*.                     in the order of creation.
*.
*. _Output parameters:
*.
*.  CHARACTER PNAMEI : Picture name .
*.                     Output parameter with the option : R or G
*.
*. N.B. PNAMEI is not used if CHOPT='L' or CHOPT='AL'
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hilun.inc"
#include "higz/hiflag.inc"
#include "higz/hiatnb.inc"
#include "higz/hiatt.inc"
#include "higz/hiflat.inc"
#include "higz/hicur.inc"
      DIMENSION IRFLAT(NBRATT),IIFLAT(NBIATT)
      EQUIVALENCE (IRFLAT(1),KMKSC),(IIFLAT(1),KPLCI)
      CHARACTER*(*) CHOPT,PNAMEI
      CHARACTER*80 STR,OLDDIR,PNAME
      CHARACTER*80 CHARS
      CHARACTER*10 CHPTNO
      DIMENSION IOPT(13),INAME(5)
      EQUIVALENCE (IOPT(1) ,IOPTM),(IOPT(2) ,IOPTD),(IOPT(3) ,IOPTS)
      EQUIVALENCE (IOPT(4) ,IOPTN),(IOPT(5) ,IOPTL),(IOPT(6) ,IOPTF)
      EQUIVALENCE (IOPT(7) ,IOPTP),(IOPT(8) ,IOPTC),(IOPT(9) ,IOPTR)
      EQUIVALENCE (IOPT(10),IOPTG),(IOPT(11),IOPTQ),(IOPT(12),IOPTA)
      EQUIVALENCE (IOPT(13),IOPTO)
*.______________________________________
*
      CALL UOPTC (CHOPT,'MDSNLFPCRGQAO',IOPT)
      PNAME=PNAMEI
      IQUEST(1)=0
*
*              Adressing pictures by number
*
      INUM=IGASCI(PNAME(1:1))
      IF(49.LE.INUM.AND.INUM.LE.57)THEN
         IF(IOPTM.NE.0)GOTO 20
         CALL IZCTOI(PNAME,INUM)
         IF(IQUEST(1).NE.0)GOTO 20
         IF(INUM.GT.NBPICT)THEN
            IF(IOPTQ.EQ.0)CALL IGERR('Picture not in memory',' ')
            RETURN
         ENDIF
         LP=LQ(LHIGZ)
         DO 10 I=1,INUM-1
            LP=LQ(LP)
   10    CONTINUE
         LCH=LQ(LP-4)
         CALL UHTOC(IQ(LCH+1),4,STR,IQ(LP+1))
         PNAME=STR(1:IQ(LP+1))
         GOTO 30
   20    IF(IOPTQ.EQ.0)CALL IGERR('PNAME must begin with a letter'
     +,                           'IZPICT')
         RETURN
      ENDIF
   30 CONTINUE
*.______________________________________
*
*              First picture
*
      IF(IOPTF.NE.0)THEN
         IF(NBPICT.NE.0)THEN
            IF(LPICT.GT.0)CALL IZWIP(LPICT)
            CALL IZSCPI(LQ(LHIGZ))
            ZFLAG=.TRUE.
            GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         ELSE
            IF(IOPTQ.EQ.0)CALL IGERR('No picture in memory',' ')
            RETURN
         ENDIF
      ENDIF
*.______________________________________
*
*              Current picture
*

      IF(IOPTC.NE.0)THEN
         IF(NBPICT.NE.0)THEN
            IF(LPICT.GT.0)CALL IZWIP(LPICT)
            IF(PNAME.EQ.' '.OR.PNAME.EQ.'*')GOTO 40
            LP=IZRPIP(PNAME)
            IF(LP.NE.0)THEN
               CALL IZSCPI(LP)
               ZFLAG=.TRUE.
               GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
            ELSE
               IF(IOPTQ.EQ.0)CALL IGERR('This picture is not in memory'
     +,                                 ' ')
               RETURN
            ENDIF
         ELSE
            IF(IOPTQ.EQ.0)CALL IGERR('No picture in memory',' ')
            RETURN
         ENDIF
      ENDIF
   40 CONTINUE
*.______________________________________
*
*              Next picture
*
      IF(IOPTN.NE.0)THEN
         IF(NBPICT.NE.0)THEN
            IF(LPICT.LT.0)THEN
               IF(IOPTQ.EQ.0)CALL IGERR('No current picture',' ')
               RETURN
            ENDIF
            CALL IZWIP(LPICT)
            IF(LQ(LPICT).NE.0)THEN
               CALL IZSCPI(LQ(LPICT))
            ELSE
               CALL IZSCPI(LQ(LHIGZ))
            ENDIF
         ELSE
            IF(IOPTQ.EQ.0)CALL IGERR('No picture in memory',' ')
            RETURN
         ENDIF
      ENDIF
*.______________________________________
*
*             Make a new picture in memory with name PNAME
*
      IF(IOPTM.NE.0)THEN
*
* HIGZ is automatically set in the state 'Z' or 'GZ'
*
         ZFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
* Automatic naming of the picture if PNAME=' ' or PNAME='*'
*
         IF((PNAME.EQ.' ').OR.(PNAME.EQ.'*'))THEN
            WRITE (CHPTNO,10100) IPICT
            IPICT=IPICT+1
            NLAST=LENOCC(CHPTNO)
            NFIRST=1
   50       IF(CHPTNO(NFIRST:NFIRST).EQ.' ')THEN
               NFIRST=NFIRST+1
               GOTO 50
            ENDIF
            PNAME='PICT'//CHPTNO(NFIRST:NLAST)
         ENDIF
*
* Automatic saving of the current picture in the RZ data base
*
         IF(LPICT.GT.0)THEN
            CALL IZWIP(LPICT)
            IF(ASFLAG)THEN
               CALL VBLANK(INAME,5)
               LOOP=(IQ(LPICT+1)+3)/4
               DO 60 I=1,LOOP
                  INAME(I)=IQ(LQ(LPICT-4)+I)
   60          CONTINUE
               LPSAV=LPICT
               CALL RZCDIR(OLDDIR,'R')
               CALL RZCDIR(HICDIR,' ')
               CALL RZOUT(IXHIGZ,LPSAV,INAME,ICYCLE,' ')
               CALL RZSAVE
               CALL RZCDIR(OLDDIR,' ')
               IF(LQ(LPSAV).NE.0)THEN
                  CALL IZSCPI(LQ(LPSAV))
               ELSE
                  CALL IZSCPI(LQ(LHIGZ))
               ENDIF
               CALL MZDROP(IXHIGZ,LPSAV,' ')
               NBPICT=NBPICT-1
               IF(NBPICT.EQ.0)THEN
                  CALL MZDROP(IXHIGZ,LHIGZ,' ')
                  LHIGZ=0
                  LPICT=-1
               ENDIF
            ENDIF
         ENDIF
*
* Create a small bank (LHIGZ) onto the all HIGZ structure is linked
*
         IF(LHIGZ.EQ.0)THEN
            CALL MZBOOK(IXHIGZ,LPICT,LHIGZ,1,'PICT',1,1,5,2,0)
         ENDIF
*
* If PNAME already exist
*
         IF(IZRPIP(PNAME).NE.0)THEN
            IF(IOPTQ.EQ.0)CALL IGERR('This picture already exist'
     +,                              'IZPICT')
            RETURN
         ENDIF
*
* Compute the link of the last picture in memory. The new picture
* is linked to the last picture in memory (LPICT=LAST)
*
         LP=LHIGZ
   70    IF(LP.NE.0)THEN
            LAST=LP
            LP=LQ(LP)
            GOTO 70
         ENDIF
         LPICT=LAST
*
* Create the Top Control Bank
*
         CALL MZBOOK(IXHIGZ,LHP,LPICT,0,'PICT',4,4,8,2,0)
         LPICT  = LHP
         LAST   = LPICT
*
* Create the Normalisation Transformation Bank number 0
*
         CALL MZBOOK(IXHIGZ,LHNT0,LPICT,-1,'HZNT',0,0,INSIZ,2,0)
*
* Create the Integer Data Bank
*
         CALL MZBOOK(IXHIGZ,LHI,LPICT,-2,'PICI',0,0,IISIZ,2,0)
*
* Create the Floating Point Data Bank
*
         CALL MZBOOK(IXHIGZ,LHF,LPICT,-3,'PICF',0,0,IFSIZ,3,0)
*
* Create the Character Data Bank
*
         CALL MZBOOK(IXHIGZ,LHC,LPICT,-4,'PICC',0,0,ISSIZ,5,0)
*
* (Number of pictures in memory) = (Number of pictures in menory) + 1
*
         NBPICT = NBPICT+1
*
* Links initialisation
*
         LHNT   = LHNT0
         LHNTLA = LHNT
*
* Number of Normalisation Transformation in the current picture
*
         INBNT  = 1
*
* The structure level number is 0
*
         INBLEV = 0
*
* Pointers initialisation
*
*   ITBPTR = Last free adress in the current transformation bank
*   INTPTR = Last free adress in the INTEGER bank data
*   IFLPTR = Last free adress in the REAL bank data
*   ISTPTR = Last free adress in the CHARACTER bank data
*   IIAPTR = Default INTEGER attributes adress
*   IRAPTR = Default REAL attributes adress
*
         ITBPTR = 10
         INTPTR = NBIATT+1
         IFLPTR = NBRATT+1
         ISTPTR = 1
         IIAPTR = 1
         IRAPTR = 1
*
* Initialisation of the real size of the bank
*
*   IRNSIZ = Real size of the current transformation bank
*   IRISIZ = Real size of the current INTEGER bank
*   IRFSIZ = Real size of the current REAL bank
*   IRSSIZ = Real size of the current CHARACTER bank
*
         IRNSIZ = INSIZ
         IRISIZ = IISIZ
         IRFSIZ = IFSIZ
         IRSSIZ = ISSIZ
*
* Names the picture. The picture name is truncated to 20 characters
*
         NCHP   = LENOCC(PNAME)
         IF(NCHP.GT.20)NCHP=20
         IF(NCHP.NE.0)THEN
            CALL UCTOH(PNAME,IQ(LHC+1),4,NCHP)
            CALL IZINCS(5)
         ENDIF
*
* Initialisation of the top control bank
*
         IQ(LPICT+1) = NCHP
         IQ(LPICT+2) = INTPTR
         IQ(LPICT+3) = IFLPTR
         IQ(LPICT+4) = ISTPTR
         IQ(LPICT+5) = IRISIZ
         IQ(LPICT+6) = IRFSIZ
         IQ(LPICT+7) = IRSSIZ
         IQ(LPICT+8) = INBNT
*
* Initialisation of the Normalisation Transformation bank 0
*
         IQ(LHNT+1) = 0
         CALL SBIT0(IQ(LHNT0+2),1)
         IQ(LHNT+3) = 1
         IQ(LHNT+4) = ITBPTR
         IQ(LHNT+5) = 0
         IQ(LHNT+8) = 1000000*NBIATT+IIAPTR
         IQ(LHNT+9) = 1000000*NBRATT+IRAPTR
         CALL IZCDA(0)
*
* Attributes flags initialisation
*
         DO 80 I=1,NBRATT
            IRFLAT(I) = 0
   80    CONTINUE
         DO 90 I=1,NBIATT
            IIFLAT(I) = 0
   90    CONTINUE
*
* Sets the current displayed picture pointers
*
         IF(GFLAG)THEN
            LPICD  = LPICT
            LDNT0  = LQ(LPICD-1)
            LDI    = LQ(LPICD-2)
            LDF    = LQ(LPICD-3)
            LDC    = LQ(LPICD-4)
            LDNT   = LDNT0
            LDNTLA = LDNT
         ENDIF
      ENDIF
*.______________________________________
*
*              Sort NT banks in the picture PNAME
*
      IF(IOPTO.NE.0)THEN
         IF(NBPICT.NE.0)THEN
*
* Order all pictures in memory
*
            IF(PNAME.EQ.'*')THEN
               LPSAV = LQ(LHIGZ)
               DO 120 I=1,NBPICT
                  LN = LQ(LPSAV-1)
                  DO 100 J=1,IQ(LPSAV+8)
                     IF(LN.EQ.0)GOTO 110
                     IQ(LN+3) = J
                     LN       = LQ(LN)
  100             CONTINUE
  110             LPSAV       = LQ(LPSAV)
                  IF(LPSAV.EQ.0)GOTO 140
  120          CONTINUE
            ENDIF
*
* Order one picture in memory
*
            IF(PNAME.EQ.' ')THEN
               LP = LPICT
            ELSE
               LP = IZRPIP(PNAME)
            ENDIF
            IF(LP.NE.0)THEN
               LN = LQ(LP-1)
               DO 130 I=1,IQ(LP+8)
                  IF(LN.EQ.0)GOTO 140
                  IQ(LN+3) = I
                  LN       = LQ(LN)
  130          CONTINUE
            ELSE
               IF(IOPTQ.EQ.0)CALL IGERR('This picture is not in memory'
     +,                                 ' ')
               RETURN
            ENDIF
         ELSE
            IF(IOPTQ.EQ.0)CALL IGERR('No picture in memory',' ')
            RETURN
         ENDIF
      ENDIF
  140 CONTINUE
*.______________________________________
*
*              Display pictures in memory
*
      IF(IOPTD.NE.0)THEN
         IF(NBPICT.NE.0)THEN
*
* Display all pictures in memory
*
            IF(PNAME.EQ.'*')THEN
               LPSAV=LQ(LHIGZ)
               DO 150 I=1,NBPICT
                  CALL IZDIPI(LPSAV)
                  LPSAV=LQ(LPSAV)
                  IF(LPSAV.EQ.0)RETURN
  150          CONTINUE
               RETURN
            ENDIF
*
* Display the current picture in memory
*
            IF(PNAME.EQ.' ')THEN
               IF(LPICT.LT.0)THEN
                  IF(IOPTQ.EQ.0)CALL IGERR('No current picture',' ')
                  RETURN
               ENDIF
               CALL IZDIPI(LPICT)
               RETURN
            ENDIF
*
* Display the picture PNAME in memory
*
            LP=IZRPIP(PNAME)
            IF(LP.NE.0)THEN
               CALL IZDIPI(LP)
            ELSE
               IF(IOPTQ.EQ.0)CALL IGERR('This picture is not in memory'
     +,                                 'IZPICT')
               RETURN
            ENDIF
         ELSE
            IF(IOPTQ.EQ.0)CALL IGERR('No picture in memory',' ')
            RETURN
         ENDIF
      ENDIF
*.______________________________________
*
*              Scratch the picture PNAME from memory
*
      IF(IOPTS.NE.0)THEN
         IF(NBPICT.NE.0)THEN
*
* Delete all pictures in memory
*
            IF(PNAME.EQ.'*')THEN
               LPSAV=LQ(LHIGZ)
               DO 160 I=1,NBPICT
                  CALL MZDROP(IXHIGZ,LPSAV,' ')
                  LPSAV=LQ(LPSAV)
                  IF(LPSAV.EQ.0)GOTO 170
  160          CONTINUE
  170          NBPICT=0
               LPICT=-1
               LPICD=-1
               CALL MZDROP(IXHIGZ,LHIGZ,' ')
               LHIGZ=0
               RETURN
            ENDIF
*
* Delete one picture in memory
*
            IF(PNAME.EQ.' ')THEN
               LP=LPICT
            ELSE
               LP=IZRPIP(PNAME)
            ENDIF
            IF(LP.NE.0)THEN
               IF(LP.EQ.LPICD)LPICD=-1
               IF(LP.EQ.LPICT)THEN
                  IF(LQ(LPICT).NE.0)THEN
                     CALL IZSCPI(LQ(LPICT))
                  ELSE
                     CALL IZSCPI(LQ(LHIGZ))
                  ENDIF
               ENDIF
               CALL MZDROP(IXHIGZ,LP,' ')
               NBPICT=NBPICT-1
               IF(NBPICT.EQ.0)THEN
                  CALL MZDROP(IXHIGZ,LHIGZ,' ')
                  LHIGZ=0
                  LPICT=-1
               ENDIF
            ELSE
               IF(IOPTQ.EQ.0)CALL IGERR('This picture is not in memory'
     +,                                 ' ')
               RETURN
            ENDIF
         ELSE
            IF(IOPTQ.EQ.0)CALL IGERR('No picture in memory',' ')
            RETURN
         ENDIF
      ENDIF
*.______________________________________
*
*              List the pictures in memory
*

      IF(IOPTL.NE.0)THEN
         IF(NBPICT.NE.0)THEN
            CALL IGSA(0)
            LP=LQ(LHIGZ)
            IF(IOPTA.NE.0)WRITE(LUNOUT,10400)
            DO 190 I=1,NBPICT
               LCH=LQ(LP-4)
               CALL UHTOC(IQ(LCH+1),4,STR,IQ(LP+1))
               CHARS=STR(1:IQ(LP+1))
*
* Full listing
*
               IF(IOPTA.NE.0)THEN
                  LN=LQ(LP-1)
                  IP=IQ(LN+3)
                  NT=IQ(LN+1)
                  IS=IQ(LP+2)+IQ(LP+3)+IQ(LP+4)
                  DO 180 J=1,IQ(LP+8)
                     IS=IS+IQ(LN+4)
                     IF(IQ(LN+3).GT.IP)THEN
                        IP=IQ(LN+3)
                        NT=IQ(LN+1)
                     ENDIF
                     LN=LQ(LN)
  180             CONTINUE
                  IF(LP.EQ.LPICT)THEN
                     WRITE(LUNOUT,10300) I,CHARS(1:20),IS,IQ(LP+8),NT
                  ELSE
                     WRITE(LUNOUT,10200) I,CHARS(1:20),IS,IQ(LP+8),NT
                  ENDIF
               ELSE
*
* Simple listing
*
                  IF(LP.EQ.LPICT)THEN
                     CHARS=CHARS(1:LENOCC(CHARS))//' <-- Current '
     +               //'Picture'
                     IF(ZFLAG)THEN
                        CHARS=CHARS(1:LENOCC(CHARS))//' (Active)'
                     ENDIF
                  ENDIF
                  WRITE(LUNOUT,10000) I,CHARS(1:LENOCC(CHARS))
               ENDIF
               LP=LQ(LP)
  190       CONTINUE
         ELSE
            IF(IOPTQ.EQ.0)CALL IGERR('No picture in memory',' ')
            RETURN
         ENDIF
      ENDIF
*.______________________________________
*
*              Print the picture data structure
*
      IF(IOPTP.NE.0)THEN
         CALL IGSA(0)
         IF(PNAME.NE.' ')THEN
            LP=IZRPIP(PNAME)
            IF(LP.NE.0)THEN
               CALL IZWIP(LP)
               CALL DZSHOW(PNAME,IXHIGZ,LP,'BV',0,0,0,0)
            ELSE
               IF(IOPTQ.EQ.0)CALL IGERR('This picture is not in memory'
     +,                                 ' ')
               RETURN
            ENDIF
         ELSE
            IF(LPICT.GT.0)THEN
               CALL IZWIP(LPICT)
               CALL DZSHOW(PNAME,IXHIGZ,LPICT,'BV',0,0,0,0)
            ELSE
               IF(IOPTQ.EQ.0)CALL IGERR('No current picture',' ')
               RETURN
            ENDIF
         ENDIF
      ENDIF
*.______________________________________
*
*              Retrieve current picture name
*

      IF(IOPTR.NE.0)THEN
         PNAMEI=' '
         IF(LPICT.LT.0)RETURN
         CALL UHTOC(IQ(LHC+1),4,STR,IQ(LPICT+1))
         PNAMEI=STR(1:IQ(LPICT+1))
      ENDIF
*.______________________________________
*
*              Retrieve displayed picture name
*
      IF(IOPTG.NE.0)THEN
         PNAMEI=' '
         IF(LPICD.LT.0)RETURN
         CALL UHTOC(IQ(LDC+1),4,STR,IQ(LPICD+1))
         PNAMEI=STR(1:IQ(LPICD+1))
      ENDIF
*
10000 FORMAT(1X,I5,': ',A)
10100 FORMAT(I10)
10200 FORMAT(1X,I5,': ',A,3X,I6,10X,I5,10X,I6)
10300 FORMAT(1X,I5,': ',A,'*',2X,I6,10X,I5,10X,I6)
10400 FORMAT(1X,/,'          PICTURE NAME      '
     +,'SIZE IN WORDS   ','NUMBER OF NT   ','CURRENT NT'/)
      END
#endif
