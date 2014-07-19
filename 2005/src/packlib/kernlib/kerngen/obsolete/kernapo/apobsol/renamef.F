*
* $Id: renamef.F,v 1.1.1.1 1996/02/15 17:51:11 mclareni Exp $
*
* $Log: renamef.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:11  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      INTEGER FUNCTION RENAMEF (CHFR,CHTO)
C
C CERN PROGLIB#         RENAMEF         .VERSION KERNAPO  1.21  900731
C ORIG. 25/07/90  JZ
C
C-    APOLLO inplementation of UNIX function
C-    but only for files, not for directories

%include '/sys/ins/base.ins.ftn'
%include '/sys/ins/pgm.ins.ftn'

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,DUMMY(36)
      CHARACTER    CHFR*(*),    CHTO*(*)
      CHARACTER    CHOLD*(256), CHNEW*(256)
      CHARACTER    CHLEAF*32
      INTEGER*2    NLEAFLG
      INTEGER*2    NFRLG,  NTOLG
      INTEGER*2    NOLDLG, NNEWLG
      INTEGER*2    NNAMLG, NOP1LG, NOP2LG

      CHARACTER    CHNAM*4, CHOP1*4, CHOP2*4
      CHARACTER    CANAM*6, CAOP1*6, CAOP2*6
      CHARACTER    CAOLD*(258), CANEW*(258)

      EQUIVALENCE (CHNAM(1:1), CANAM(3:3))
      EQUIVALENCE (CHOP1(1:1), CAOP1(3:3))
      EQUIVALENCE (CHOP2(1:1), CAOP2(3:3))
      EQUIVALENCE (CHOLD(1:1), CAOLD(3:3))
      EQUIVALENCE (CHNEW(1:1), CANEW(3:3))

      EQUIVALENCE (NNAMLG, CANAM)
      EQUIVALENCE (NOP1LG, CAOP1)
      EQUIVALENCE (NOP2LG, CAOP2)
      EQUIVALENCE (NOLDLG, CAOLD)
      EQUIVALENCE (NNEWLG, CANEW)

      DIMENSION    LARGV(5)

      ISTAT = -1
      NFR   = ICFIND (' ', CHFR,1,LEN(CHFR)) - 1
      IF (NFR.LE.0)                GO TO 39

      NTO   = ICFIND (' ', CHTO,1,LEN(CHTO)) - 1
      IF (NTO.LE.0)                GO TO 39

C----              Expand FROM path-name, check file exists

      RENAMEF = -1
      NFRLG  = NFR
      CALL NAME_$GET_PATH (CHFR,NFRLG,CHOLD,NOLDLG,ISTAT)
      IF (ISTAT.NE.0)              GO TO 39

C----              Expand TO path-name, check file exists

      NTOLG  = NTO
      CALL NAME_$GET_PATH (CHTO,NTOLG,CHNEW,NNEWLG,ISTAT)
      IF (ISTAT.NE.0)              GO TO 24

C--       Delete file

      CALL NAME_$DELETE_FILE (CHNEW,NNEWLG,ISTAT)
      GO TO 31

C--       Construct expansion

   24 JSLNEW = ICFIND ('/',CHTO,1,NTO)
      IF (NGSLAT.EQ.0)             GO TO 25
      CHNEW  = CHTO
      NNEWLG = NTOLG
      GO TO 31

   25 CALL NAME_$GET_WDIR (CHNEW,NNEWLG,ISTAT)
      IF (ISTAT.NE.0)              GO TO 39
      NNEWLG = NNEWLG + 1
      CHNEW(NNEWLG:NNEWLG) = '/'
      CHNEW(NNEWLG+1:NNEWLG+NTOLG) = CHTO(1:NTOLG)
      NNEWLG = NNEWLG + NTOLG

C----              Decide  copy | rename

   31 NOLD = NOLDLG
      NNEW = NNEWLG
      CALL CUTOL (CHOLD(1:NOLDLG))
      CALL CUTOL (CHNEW(1:NNEWLG))

      IF (NOLD.EQ.NNEW)  THEN
          IF (CHOLD(1:NOLD).EQ.CHNEW(1:NNEW))    RETURN
        ENDIF

      JSLO = ICFILA ('/',CHOLD,1,NOLD)
      JSLN = ICFILA ('/',CHNEW,1,NNEW)

      IF (JSLO.NE.JSLN)                    GO TO 41
      IF (CHOLD(1:JSLN).NE.CHNEW(1:JSLN))  GO TO 41

C----              RENAMEF file

      NLEAFLG = NNEW - JSLN
      IF (NLEAFLG.LE.0)                 RETURN

      CHLEAF = CHNEW(JSLN+1:JSLN+NLEAFLG)

      CALL NAME_$CNAME (CHOLD,NOLDLG,CHLEAF,NLEAFLG,ISTAT)
   39 RENAMEF = ISTAT
      RETURN

C----              Copy file

   41 CHNAM = 'cpf'
      CHOP1 = '-r'
      CHOP2 = '-pdt'

      NNAMLG = 3
      NOP1LG = 2
      NOP2LG = 4

      LARGV(1) = IADDR (CANAM)
      LARGV(2) = IADDR (CAOLD)
      LARGV(3) = IADDR (CANEW)
      LARGV(4) = IADDR (CAOP1)
      LARGV(5) = IADDR (CAOP2)

      CALL PGM_$INVOKE ('/com/cpf',INT2(8), INT2(5),LARGV,
     +                  0,0, PGM_$WAIT, IHDL, ISTAT)
      RENAMEF = ISTAT
      IF (ISTAT.NE.0)              RETURN

C--       delete the original

      CALL NAME_$DELETE_FILE (CHOLD,NOLDLG,ISTAT)
      RETURN
      END
#ifdef CERNLIB_CCGEN_RENAMEF
#undef CERNLIB_CCGEN_RENAMEF
#endif
#ifdef CERNLIB_TCGEN_RENAMEF
#undef CERNLIB_TCGEN_RENAMEF
#endif
