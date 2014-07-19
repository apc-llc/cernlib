*
* $Id: hbnt.F,v 1.1.1.1 1996/01/16 17:07:56 mclareni Exp $
*
* $Log: hbnt.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:56  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/14 07/10/94  15.04.56  by  Fons Rademakers
*-- Author :    Fons Rademakers   16/12/91
      SUBROUTINE HBNT(IDD, CHTITL, CHOPT1)
*.==========>
*.           Booking of a new (variable row length) n-tuple
*.
*.
*..=========> ( A.A.Rademakers )
#if defined(CERNLIB_HNTUPDOC)
New Ntuple data structure (Fig. A).
 
                                  |
                                  |  LCID   (master is LCDIR-2)
                                  |
    -6   -5   -4   -3   -2   -1   V
 +-------------------------------------------------------------------+
 |     |    |    |    |    |    | BITS, NDIM, NOENT, etc (see T.A)   |-->
 +-------------------------------------------------------------------+
    |     |    |    |    |    |     -7 LLBLK pointer to last block
 ...|.....|....|....|....|....|...............................
    |     |    |    |    |    |                              .
    V     |    |    |    |    V LBLOK                        V
  struc   |    |    |    |  +----------------------------+   +------------+
  link    |    |    |    |  | Ntuple block bank (see F.B)|-->| next block |
  reserved|    |    |    |  +----------------------------+   +------------+
          |    |    |    |
          |    |    |    | LCHAR
          V    |    |    |    +----------------------------------------------+
        LFUNC  |    |    +--->| char bank pointed at by variable description |
        used   |    |         +----------------------------------------------+
        for    |    | LINT
        quadric|    |    +-------------------------------------------------+
        fit    |    +--->| integer bank pointed at by variable description |
               |         +-------------------------------------------------+
               | LREAL
               |    +----------------------------------------------+
               +--->| real bank pointed at by variable description |
                    +----------------------------------------------+
 
 Total of 12 links, 6 structural and 6 reference links.
 Reference link -8 is used by HFNTB and HGNTB and points to last used LBLOK.
 Reference links -9 to -12 are reserved.
 
 
                                 Table A
 ************************************************************************
 * word   *  tag    *               content                             *
 ************************************************************************
 *        *         *                                                   *
 *   1    *  BITS   *     Status word (1)                               *
 *   2    *  NDIM   *     Number of variables in the Ntuple             *
 *   3    *  NOENT  *     Total number of entries                       *
 *   4    *  NPRIME *     Number of words for primary allocation (2)    *
 *   5    *  IFCHAR *     Pointer to 1st free word in LCHAR             *
 *   6    *  IFINT  *     Pointer to 1st free word in LINT              *
 *   7    *  IFREAL *     Pointer to 1st free word in LREAL             *
 *   8    *  NWTITL *     Number of words in the title                  *
 *   9    *  ITIT1  *     Internal pointer to the first word of title   *
 *  10    *  NBLOK  *     Number of blocks in the Ntuple                *
 *  11    *  IFTMP  *     Pointer to 1st free word in LTMP              *
 *  12    *  ID     *     Id used to write Ntuple extensions to disk    *
 *  13    *  NCHRZ  *     Number of characters in RZ dir                *
 *  14    *  CHRZ1  *     1st word of RZ directory                      *
 *  15    *  CHRZ2  *     2nd word, etc                                 *
 *        *   ...   *                                                   *
 * ITIT1  *  TITLE1 *     1st word of title                             *
 *        *   ...   *                                                   *
 ************************************************************************
 
 (1)   Status word: bit 4:   1 if Ntuple
                    bit 5,6,7:  Number of arrays in ONTs
                    bit 8:   if Character block in ONTs
 (2)   If NPRIME < 0 then memory resident Ntuple.
 (*)   Bit 1 in status word of first LBLOK is set when the Ntuple header
       has been modified and has to be written to disk.
 (**)  Bit 2 in status word of first LBLOK is set when the Ntuple header
       has been extended with new columns and has to be written to disk.
 (***) Bit 3 is set when Ntuple is read back by PAW.
 
 
 
Ntuple block (column) data structure (Fig B).
 
                                  |
                                  |
                                  |  LBLOK
    -6   -5   -4   -3   -2   -1   V
  +-----------------------------------------------------------+
  |    |    |    |    |    |    | BITS, NDIM, etc (see T.B)   |-->
  +-----------------------------------------------------------+
     |    |    |    |    |    |
     |    |    |    |    |    +----------+
     |    |    |    |    |               | LNAME
     V    V    V    V    V   -NDIM...-1  V
    ref link   struc link    +------------------------------------------------+
    reserved   reserved      |    |    | Ntuple variable description (see T.C)|
                             +------------------------------------------------+
                             NDIM reference links to the corresponding
                             contents banks in LBUF
 
 
 
                                Table B
 ************************************************************************
 * word   *  tag    *               content                             *
 ************************************************************************
 *        *         *                                                   *
 *   1    *  BITS   *     Status word                                   *
 *   2    *  NDIM   *     Number of variables in the block              *
 *   3    *  NOENT  *     Number of entries in block                    *
 *   4    *  IFNAME *     Pointer to 1st free word in LNAME             *
 *   5    *         *     Reserved                                      *
 *   6    *         *     Reserved                                      *
 *   7    *         *     Reserved                                      *
 *   8    *  IBLOK1 *     1 word of block name                          *
 *   9    *  IBLOK2 *     2 word of block name                          *
 ************************************************************************
 
 
 
                                Table C
 ************************************************************************
 * word *  tag    *                 content                             *
 ************************************************************************
 *      *         *                                                     *
 *   1  *  DESC   *     Variable descriptor (see T.D)                   *
 *   2  *  LNAME  *     Length of variable name                         *
 *   3  *  NAME   *     Pointer to the variable name in LCHAR           *
 *   4  *  RANGE  *     Pointer to the variable range, for a real       *
 *      *         *     two reals in LREAL for an (unsigned) integer    *
 *      *         *     two integers in LINT                            *
 *   5  *  NRZB   *     Number of RZ records or memory blocks           *
 *   6  *  LCONT  *     Link position of contents bank in LBUF          *
 *   7  *  IFCONT *     Pointer to 1st (partially) free word in the     *
 *      *         *     contents bank                                   *
 *   8  *  IFBIT  *     Pointer to 1st free bit in IFCONT               *
 *   9  *  IBANK  *     Contents buffer currently in memory             *
 *  10  *  ITMP   *     Pointer to tmp storage in LTMP                  *
 *  11  *  ARIND  *     Pointer to the array indices (as many as the    *
 *      *         *     variable has dimensions) in LINT. If the        *
 *      *         *     dimension <0 then fixed length dimension,       *
 *      *         *     otherwise a pointer to the descriptor of the    *
 *      *         *     index variable                                  *
 *  12  *  NADDR  *     Address of first word of variable               *
 ************************************************************************
 
 
 
                      Table D: Variable descriptor
       +----------------------------------------------------------------+
       | status |              | dimension | type | size | packing bits |
       +----------------------------------------------------------------+
       |        |              |           |      |      |              |
 bits 32       28             21          18     14      8              1
 
 
 status        bit 1       1 if index variable
               bit 2-5     unused
 
 dimension     <=7         number of dimensions if variable is an array
 
 type          1           floating-point variable
               2           integer variable
               3           unsigned integer variable
               4           logical variable
               5           character variable
 
 size          1-63        size of the variable in bytes (size of
                           single element in case of vector)
 
 packing bits  <=8*size    number of bits in which the variable can be packed
 
 
 
 LBUF buffer management structure. LCDIR-4 is the master pointer.
 LBUF points to the buffer structure of the current Ntuple (Fig. C).
 
                                  |
                                  |
                                  |  LBUF
  -NDIM  ...  ...  -3   -2   -1   V
 +-----------------------------------------------------+
 |     |    |    |    |    |    | BITS, NDIM (see T.E) |-->
 +-----------------------------------------------------+
    |     |    |    |    |    |
    |     |    |    |    |    |
    |     |    |    |    |    |
    |     V    V    V    |    |
    |                    |    |   +------------------------+
    |                    |    +-->| Contents of variable 1 |-->(when memory
    |                    |        +------------------------+    resident)
    |                    |
    |                    |    +------------------------+
    |                    +--->| Contents of variable 2 |-->
    |                         +------------------------+
    |
    |       +---------------------------+
    +------>| Contents of variable NDIM |-->
            +---------------------------+
 
 ID is in IQ(LBUF-5)
 Bit 1 in the status word of every contents buffer is set when the buffer
 has not yet been written to disk.
 
 
                                 Table E
 ************************************************************************
 * word *  tag    *               content                               *
 ************************************************************************
 *      *         *                                                     *
 *   1  *         *     Reserved                                        *
 *   2  *         *     Reserved                                        *
 ************************************************************************
 
 
 LCDIR-5 is the master pointer of the temporary workspace used during
 reading of the Ntuple. LTMP points to the tmp buffer of the current
 Ntuple. LQ(LTMP-1) is used by the fast reading routine HGNTF (Fig. D).
 
                 |
                 |
                 |  LTMP
     -2    -1    V
   +----------------------------------------------------------------------+
   |     |     | Temporary data used during filling and reading (see T.F) |-->
   +----------------------------------------------------------------------+
      |     |
      |     |
      V     +-----------+
 ref. link              | LTMP1
 to LCID    -NDIM...-1  V
 of current    +--------------------------------------------------------+
 Ntuple        |    |    | Variable index and offset in LNAME (see T.G) |
               +--------------------------------------------------------+
               NDIM reference links to the corresponding LBLOK banks
 
 
 ID is in IQ(LTMP-5)
 
 
                                  Table F
 ************************************************************************
 * word *  tag    *               content                               *
 ************************************************************************
 *      *         *                                                     *
 *   1  *         *     Last handled event                              *
 *      *         *     *** For every variable 6 words (ZNTMP) ***      *
 *   2  *         *     Contents bank currently in memory (reading)     *
 *   3  *         *     Pointer to first free word in current bank (r)  *
 *   4  *         *     Pointer to the first free bit in free word (r)  *
 *   5  *         *     Number of bits left in bank (r)                 *
 *   6  *         *     When index variable value of previous row (r)   *
 *   7  *         *     When index variable value of current row (r)    *
 ************************************************************************
 
 
                                  Table G
 ************************************************************************
 * word *  tag    *               content                               *
 ************************************************************************
 *      *         *                                                     *
 *   1  *         *     Number of variables to be read out              *
 *      *         *     *** For every variable 3 words (ZNTMP1) ***     *
 *   2  *         *     Index of variable in LNAME bank                 *
 *   3  *         *     Offset of variable in LNAME bank (INDX-1)*ZNADDR*
 *   4  *         *     Offset in dynamically allocated buffer (only    *
 *      *         *     used via HGNTBF) otherwise 0                    *
 ************************************************************************
#endif
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcform.inc"
#include "hbook/hcset.inc"
*
      INTEGER       IDD
      CHARACTER*(*) CHTITL, CHOPT1
      CHARACTER*80  CHRZ
      CHARACTER*8   CHOPT
*
      IERR = 0
      IF (IDD.LE.0) THEN
         CALL HBUG('ID<1 is an illegal identifier','HBNT',IDD)
         RETURN
      ENDIF
      IF (IDD.GE.10000) THEN
         CALL HBUG('ID>9999 is an illegal identifier','HBNT',IDD)
         RETURN
      ENDIF
*
      IF (CHOPT1 .EQ. ' ') THEN
         CHOPT = 'D'
      ELSE
         CHOPT = CHOPT1
      ENDIF
*
*             Check if ID already in the table
*
      ID     = IDD
      NRHIST = IQ(LCDIR+KNRH)
      IDPOS  = LOCATI(IQ(LTAB+1),NRHIST,ID)
      IF (IDPOS.GT.0) THEN
         CALL HBUG('+Already existing N-tuple replaced','HBNT',IDD)
         CALL HDELET(IDD)
         NRHIST = IQ(LCDIR+KNRH)
         IDPOS  = -IDPOS+1
      ENDIF
*
*             Title transform from various accepted formats
*
      CALL HBTIT(CHTITL,NWTITL,NCHT)
*
      CALL HCDIR(CHRZ, 'R')
      IF (INDEX(CHOPT,'M') .NE. 0) THEN
         NCHRZ = 0
      ELSE
         NCHRZ = LENOCC(CHRZ)
      ENDIF
      NCHR  = 20
      ITIT1 = ZNCHRZ+1+NCHR
*
      NWPRIM = IBSIZE
*
      NWID  = NWTITL+ITIT1-1
      NWID2 = ZIBLOK+1
      NWH   = 128
      NWN   = 128
      NTOT  = NWID+NWID2+3*NWH+NWN+6*33
*
*             Enough space left ?
*
      CALL HSPACE(NTOT+1000,'HBNT',IDD)
      IF (IERR.NE.0) GOTO 99
*
*             Enter ID in the list of ordered IDs
*
      IDPOS=-IDPOS+1
      IF (NRHIST.GE.IQ(LTAB-1)) THEN
         CALL MZPUSH(IHDIV,LTAB,500,500,'I')
      ENDIF
      DO 10 I = NRHIST, IDPOS, -1
         IQ(LTAB+I+1) = IQ(LTAB+I)
         LQ(LTAB-I-1) = LQ(LTAB-I)
  10  CONTINUE
*
*             Build ntuple data structure
*
      IF (LIDS.EQ.0) THEN
         CALL MZBOOK(IHDIV,LIDS,LCDIR,-2,'HIDT',ZLINK+6,ZLINK,
     +               NWID,IONT,0)
         LCID = LIDS
      ELSE
         LLID = LQ(LCDIR-9)
         CALL MZBOOK(IHDIV,LCID,LLID,  0,'HIDT',ZLINK+6,ZLINK,
     +               NWID,IONT,0)
      ENDIF
      LQ(LCDIR-9) = LCID
*
      CALL MZBOOK(IHDIV,LBLOK,LCID,-1,'HBLK',ZLINK,ZLINK-2,NWID2,IOBL,0)
      CALL MZBOOK(IHDIV,LCHAR,LCID,-2,'HCHA',0,0,NWH,5,0)
      CALL MZBOOK(IHDIV,LINT, LCID,-3,'HINT',0,0,NWH,2,0)
      CALL MZBOOK(IHDIV,LREAL,LCID,-4,'HREA',0,0,NWH,3,0)
      LQ(LCID-7) = LBLOK
*
      CALL MZBOOK(IHDIV,LNAME,LBLOK,-1,'HNAM',1,0,NWN,2,0)
*
      IQ(LCID-5)     = ID
      IQ(LTAB+IDPOS) = ID
      LQ(LTAB-IDPOS) = LCID
*
      CALL SBIT1(IQ(LCID+ZBITS),4)
      IQ(LCID+ZNPRIM) = NWPRIM
      IF (INDEX(CHOPT,'M') .NE. 0) THEN
         IQ(LCID+ZNPRIM) = -NWPRIM
      ENDIF
      IQ(LCID+ZIFCHA) = 1
      IQ(LCID+ZIFINT) = 1
      IQ(LCID+ZIFREA) = 1
      IQ(LCID+ZIFTMP) = 2
      IQ(LCID+ZID)    = ID
      IQ(LCID+ZNWTIT) = NWTITL
      IQ(LCID+ZITIT1) = ITIT1
      IQ(LCID+ZNCHRZ) = NCHRZ
*
      IQ(LBLOK+ZIFNAM) = 1
*
*           Copy RZ path and title
*
      IF (NCHRZ.NE.0) THEN
         IF (CHRZ(1:6).EQ.'//PAWC') THEN
            IQ(LCID+ZNPRIM) = -NWPRIM
            IQ(LCID+ZNCHRZ) = 0
            CHRZ = ' '
            PRINT *, '*** Warning: N-tuple will be memory resident'
            GOTO 20
         ENDIF
         CALL UCTOH(CHRZ,IQ(LCID+ZNCHRZ+1),4,80)
      ENDIF
  20  IF (NWTITL.NE.0) THEN
         CALL UCTOH(CHTITL,IQ(LCID+ITIT1),4,NCHT)
      ENDIF
*
*-- set bit 1 in LBLOK status word when the N-tuple has changed
*-- when this bit is set disk resident N-tuple should be updated on disk
*
      CALL SBIT1(IQ(LBLOK),1)
*
      NRHIST = NRHIST+1
      IQ(LCDIR+KNRH) = NRHIST
*
  99  RETURN
      END
