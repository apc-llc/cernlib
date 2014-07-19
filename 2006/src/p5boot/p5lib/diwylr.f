CDECK  ID>, DIWYLR.
      SUBROUTINE DIWYLR (LUP,LEVEL)

C-    generate the Wylbur command to re-order
C-        LEVEL = 0:  the decks of the patch at LUP
C-                1:  the patches of the PAM at LUP
C.    started 18-june-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
     +,         LEXP,LLPAST,LQPAST, LQUSER(4), LHASM,LRPAM,LPAM, LQINCL
     +,         LACRAD,LARRV, LPCRA,LDCRAB, LEXD,LDECO, LCRP,LCRD, LSERV
     +, INCRAD, IFLGAR, JANSW, IFMODIF, IFALTN
     +, JDKNEX,JDKTYP, JSLZER,NSLORG,JSLORG
     +, MOPTIO(34), MOPUPD, NCLASH, IFLMERG,IFLDISP, NSLFRE,NTXFRE
     +, NVGAP(4), NVGARB(6), NVIMAT(4), NVUTY(4),  LASTWK
     +,   INMEM(2),NSLTOT(2),NSLMAX(2),NTXMAX(2),JSLTTF(2), IFSTRUC
     +,   NOBJTT(3,2),NOBJMAT, MDELADD(2,2), LUNCRA,NLSENT,   LASTDI
C--------------    End CDE              --------------------------------
      CHARACTER    LINE*512,      COL(512)*1
      CHARACTER    LONG*8192,     LONC(8192)*1
      EQUIVALENCE (LINE,SLLINE), (COL,SLLINE)
      EQUIVALENCE (LONG,TEXT),   (LONC,TEXT)
      CHARACTER    EQU*3
      LOGICAL      PRINT

C-    this is the maximum number of line-range elements on
C-    a command line which Wylbur can handle:
      PARAMETER   (NSIZELI=20)


      EQU = ' d='
      IF (LEVEL.NE.0)  EQU= ' p='

 9001 FORMAT (A)
 9002 FORMAT (3X,A)

      PRINT = LUNCRA.EQ.IQPRNT
                  ASSIGN 9001 TO IFMT
      IF (PRINT)  ASSIGN 9002 TO IFMT

C----         look for the next re-ordering group

      LGO = LQ(LUP-4)
   21 LGO = LQSTEP (LGO, IQ(LGO+9)+1)
      IF (LGO.EQ.0)                RETURN

      LMIN = LGO
      LMAX = LGO
      JORD = IQ(LGO+5)

      MINCUR = JORD
      MAXCUR = JORD

C--       scan starting from the string at LGO

   23 MINORD = MINCUR
      MAXORD = MAXCUR
      LBK = LGO

C--       find the last string with ordinal number < MAXORD

   24 LBK = LQSTEP (LBK, IQ(LBK+9)+1)
      IF (LBK.EQ.0)                GO TO 26
      JORD = IQ(LBK+5)
      IF (JORD.GE.MAXCUR)          GO TO 24
      LLAST = LBK
      IF (JORD.GE.MINCUR)          GO TO 24
      MINCUR = JORD
      LMIN   = LBK
      GO TO 24

C--       no re-ordering of string at LGO  if no such string found

   26 IF (LMIN.EQ.LGO)             GO TO 21
      IF (MINCUR.EQ.MINORD)        GO TO 41

C--       find the string with the highest ordinal number in (LGO | LLAST)

      LBK = LGO
   27 LBK = LQSTEP (LBK, IQ(LBK+9)+1)
      IF (LBK.EQ.LLAST)            GO TO 29
      JORD = IQ(LBK+5)
      IF (JORD.LE.MAXCUR)          GO TO 27
      MAXCUR = JORD
      LMAX   = LBK
      GO TO 27

C--       if the maximum has increased: scan again

   29 IF (MAXCUR.NE.MAXORD)        GO TO 23

C----         found the re-ordering group from LGO to LLAST

C--       construct:  range p=pname,d=dfirst-dlast
C--              or:  range p=pfirst-plast

   41 IXFI = IQ(LMIN+4)
      LBK  = LQSTEP (LMAX,IQ(LMAX+9))
      IXLA = IQ(LBK+4)
      NLRE = 0

      IF (PRINT)  WRITE (IQPRNT,9001)
      WRITE (LUNCRA,IFMT) '#     change the order'

C-            _:.=+=.:_1
      LINE = 'range p='
      JPUT = 9

      IF (LEVEL.EQ.0)  THEN
          CALL NA_GET (IQ(LUP+4),LINE,JPUT)
          JPUT = NESLAT
          LINE(JPUT:JPUT+2) = ',d='
          JPUT = JPUT + 3
        ENDIF

      CALL NA_GET (IXFI,LINE,JPUT)
      COL(NESLAT) = '-'
      JPUT = NESLAT + 1
      CALL NA_GET (IXLA,LINE,JPUT)
      NRANGE = NESLAT - 1

C--       construct:  move,o,n d=d1-d2 d=d3-d4 ... to f

C-                  _:.=+=.:_1
      LONG(1:10) = 'move,o,n  '
      JPUT = 10
      LBK  = LGO
   44 NLEN = IQ(LBK+9)
      IXFI = IQ(LBK+4)
      NLRE = NLRE + 1

      LONG(JPUT:JPUT+2) = EQU
      CALL NA_GET (IXFI,LONG,JPUT+3)
      JPUT = NESLAT

      LE = LBK
      IF (NLEN.NE.0)  THEN
          LONC(JPUT) = '-'
          LE = LQSTEP (LE,NLEN)
          CALL NA_GET (IQ(LE+4),LONG,JPUT+1)
          JPUT = NESLAT
        ENDIF
      IF (LBK.EQ.LLAST)            GO TO 47
      LBK = LQ(LE-1)
      GO TO 44

C! 47 IF (JPUT.GT.25)              GO TO 61
   47 IF (JPUT.GT.250)             GO TO 61
      IF (NLRE.GE.NSIZELI)         GO TO 61
      LONG(JPUT:JPUT+4) = ' to f'
      NPUT = JPUT + 4
      WRITE (LUNCRA,IFMT) LINE(1:NRANGE)
      WRITE (LUNCRA,IFMT) LONG(1:NPUT)
      NLSENT = NLSENT + 2
      LGO = LLAST
      GO TO 21

C--       resulting command line too long, operate in pieces

   61 JEND = JPUT - 1
      WRITE (LUNCRA,IFMT) 'range'
      WRITE (LUNCRA,IFMT) 'end end-marker'
      WRITE (LUNCRA,IFMT) 'set val w11 *'
      WRITE (LUNCRA,IFMT) LINE(1:NRANGE)
      NLSENT = NLSENT + 4

      JTAK = 10
   62 LINE = 'copy,n'
      JPUT = 8
      NLRE = 0
   63 JTAK = ICNEXT (LONG,JTAK,JEND)
      N    = NDSLAT
      IF (JPUT+N.GE.250)           GO TO 64
      IF (NLRE+1.EQ.NSIZELI)       GO TO 64

      NLRE = NLRE + 1
      LINE(JPUT:JPUT+N-1) = LONG(JTAK:JTAK+N-1)
      JTAK = JTAK + N + 1
      JPUT = JPUT + N + 1
      IF (JTAK.LT.JEND)            GO TO 63

   64 LINE(JPUT:JPUT+4) = 'to *+'
      NPUT = JPUT + 4
      WRITE (LUNCRA,IFMT) LINE(1:NPUT)
      NLSENT = NLSENT + 1
      IF (JTAK.LT.JEND)            GO TO 62

      WRITE (LUNCRA,IFMT) 'set val w12 *'
      WRITE (LUNCRA,IFMT) 'copy,o,n :w11+/:w12 to f'
      WRITE (LUNCRA,IFMT) 'del -nol :w11/:w12'
      NLSENT = NLSENT + 3
      LGO = LLAST
      GO TO 21

      END
