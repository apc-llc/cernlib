*
* $Id: cdinit.F,v 1.1.1.1 1996/02/28 16:24:13 mclareni Exp $
*
* $Log: cdinit.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:13  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDINIT(IDIV,LURZ,LUFZ,TOPN,NPAIR,NREC,NTOP,CHOP,IRC)
*     ===============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDINIT (IDIV,LURZ,LUFZ,TOPN,NPAIR,NREC,NTOP,CHOP,IRC*)  *
*                                                                      *
*   Initializes of the Data Base Package (for each file)               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDIV     User Division                                           *
*     LURZ     RZ file unit                                            *
*     LUFZ     FZ file unit                                            *
*     TOPN     Name of the Top Directory                               *
*     NPAIR    Number of key pairs used for setting validity range of  *
*              objects (to be given while creating new database)       *
*     NREC     Number of records for primary allocation (for RZMAKE)   *
*              If less than or equal to 0 use existing RZ file (RZFILE)*
*     NTOP     Serial number of top directory (if 0, automatically     *
*              given by the package)                                   *
*     CHOP     Character Option : Same as in RZFILE / RZMAKE           *
*          A   All validity range key pairs on equal footing           *
*              (default is to assume hierarchy among the keys)         *
*          Z   Reinitialize the random access file (use RZMAKE)        *
*              (also NREC > 0 is required)                             *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDNEW,  CDOPEN                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = -1 : Invalid top directory name                      *
*               = -2 : The file is already open with correct LURZ and  *
*                      TOPN                                            *
*               = -3 : The file is already open with wrong LURZ or TOPN*
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__ONLINE))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))
*               = -5 : Invalid process name in Online context          *
*               = -6 : Error in IC_BOOK for booking the CACHE          *
*               = -7 : Error in CC_SETUP for reserving the CLUSCOM     *
#endif
#if defined(CERNLIB__P3CHILD)
*               = -9 : Unable to open FZ communication channel         *
*               = -10: Host unable to open RZ file                     *
#endif
*                                                                      *
************************************************************************
*
#include "zebra/zunit.inc"
*
#include "hepdb/caopti.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__ONLINE))&&(defined(CERNLIB__SERVER))
#include "hepdb/cbsrvr.inc"
#include "hepdb/crccom.inc"
*           Cache stuff
      INTEGER         ICACH(4)
      BYTE            BCACH(16)
      EQUIVALENCE     (BCACH, ICACH)
      CHARACTER       DBCSN*2, DBCLN*4, DBCCM*9
#endif
      PARAMETER       (NOPRZ=8)
      CHARACTER       CHOP*(*), TOPN*(*)
      CHARACTER       CHOPT*8, COPRZ(NOPRZ)*1, CFOR*32
      INTEGER         IOPRZ(NOPRZ), IACRZ(NOPRZ)
      SAVE            INIT, COPRZ, IACRZ
*
      DATA            INIT /0/
*     Valid RZ options
      DATA            COPRZ /'1', 'D', 'L', 'M', 'S', 'U', 'X', 'C'/
*     Valid RZ options for public mode
      DATA            IACRZ /  0,   0,   1,   0,   0,   0,   1,   1/
*
*     ------------------------------------------------------------------
*
C ACP_data_retrieval_start
 
      IQ10   = IQUEST(10)
      IQ11   = IQUEST(11)
      LURZCD = LURZ
      IOPTO  = 0
*     CALL UOPTC (CHOP, 'Z', IOPTZ)
      IOPTZ  = INDEX(CHOP,'Z')
      IF (NREC.GT.0.AND.IOPTZ.NE.0) THEN
        NRECS = NREC
        IOPTN = 1
      ELSE
        IOPTN = 0
        NRECS = 0
      ENDIF
      DO I = 1, NOPRZ
        IF (I.EQ.1) THEN
          CHOPT  = COPRZ(I)
        ELSE
          CHOPT  = CHOPT(1:I-1)//COPRZ(I)
        ENDIF
      ENDDO
      CALL UOPTC (CHOP, CHOPT(1:NOPRZ), IOPRZ)
      IF (IOPRZ(1).NE.0.OR.IOPRZ(6).NE.0) IOPTO = 1
*
* *** Remove imbedded blanks from the top directory name
*
      CALL CDSBLC (TOPN, TOPNCD, NCHRCD)
      IF (NCHRCD.EQ.0) THEN
        IRC    = -1
#if defined(CERNLIB__DEBUG)
        TOPNCD = TOPN
        CALL CDPRNT (IQPRNT, '(/,'' CDINIT : Illegal Top Directory '//
     +       'Name '//TOPNCD//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** First pass initialization
*
      IF (INIT.EQ.0) THEN
 
        CALL UOPTC (CHOP, 'E', IOPTE)
        CALL UOPTC (CHOP, 'Q', IOPTQ)
*
*  **   DB System initialization
*
        CALL CDFRST (IDIV)
 
        IF(IOPTQ.EQ.0) CALL CDVERS(' ',' ','P')
*
*  **   Request expansion of system division
*
        IF(IOPTE.NE.0) CALL CDEXPD(0,NDMX)
 
        INIT   = 1
      ENDIF
*
      IOPPCD = 0
      CALL UOPTC (CHOP, 'S', IOPSCD)
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
* *** Check if it is Server environment
*
      CALL UOPTC (CHOP, 'O', IOPO)
      CALL UOPTC (CHOP, 'P', IOPPCD)
      CALL UOPTC (CHOP, 'R', IOPR)
#endif
#if defined(CERNLIB__P3CHILD)
      IF (IOPTO.NE.0.OR.IOPTN.NE.0) IOPPCD = 1
#endif
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
      IF (INSRCD.EQ.0) THEN
#endif
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX)||defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB_ONLINE))
*
* ***   Server environment, Public mode
*
        IF (IOPPCD.NE.0.AND.IOPR.EQ.0) THEN
#endif
#if defined(CERNLIB__P3CHILD)
*
*  **     Open the FZ Communication Channel
*
          CALL APOPCH (LODBP3, 1, IRET)
          IF (IRET.NE.0) THEN
            IRC        = -9
            IQUEST(11) = IRET
#endif
#if (defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__DEBUG))
            CALL CDPRNT (LPRTCD, '(/,'' CDINIT : Cannot open FZ '//
     +           'channel'')', IRET, 0)
#endif
#if defined(CERNLIB__P3CHILD)
          ENDIF
          INSRCD = 1
        ENDIF
#endif
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
*
*  **     Open the journal file to DBSERVER
*
          LUF    = LUFZ
          LUFMCD = LUFZ
          CALL CDFZLO (LUF, IFND)
          IF (IFND.EQ.0) THEN
            CALL CDFOPN (LUF, IRC)
            IF (IRC.NE.0) THEN
              IF (IDEBCD.GE.0) CALL CDPRNT (LPRTCD, '(/,'' CDINIT : '//
     +        'Error in CDFOPN'')', IARGCD, 0)
              GO TO 999
            ENDIF
            CALL FZFILE (LUF, 0, 'AOQ')
          ENDIF
          INSRCD = 1
        ENDIF
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
*
* ***   Server environment, Owner mode
*
        IF (IOPO.NE.0) THEN
*
*  **     Open a flat memory for communication via FZ
*
          CALL FZFILE (LUFMCD, 0, 'MNUIQ')
          CALL FZMEMO (LUFMCD, QVECCR, NMAXCR)
          INSRCD = 1
*
* ***   Server environment, Public mode
*
        ELSE IF (IOPPCD.NE.0) THEN
*
*  **     Open a flat memory for communication via FZ
*
          CALL FZFILE (LUFMCD, 0, 'MNUOQ')
          CALL FZMEMO (LUFMCD, QVECCR, NMAXCR)
*
*  **     Book cache as  a client
*
          CALL CD_CHK_CLIENT (DBCSN, DBCLN, DBCCM, IERR)
          IF (IERR.NE.0) THEN
            IRC    = -5
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDINIT : Ill'//
     +      'egal Cluscom name '//DBCCM//''')', IARGCD, 0)
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
            GO TO 999
          ENDIF
*
*  **     Directory name for MAPCOM files is defined through
*  **     db long name. Example 'MUCH$DBMCOM:
*
          DIRECB = DBCLN//'$DBMCOM:'
*
*  **     Cache name is the same for the server and its clients
*  **     Example H__DB_CACHE (always 11 characters)
*
          CACHCB = DBCSN(1:1)//'__DB_CACHE'
          CALL IC_BOOK (CACHCB, ICACH, 'C', IERR)
          IF (IERR.NE.0) THEN
            IRC        = -6
            IQUEST(11) = IERR
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDINIT : Err'//
     +      'or '',I10,'' in IC_BOOK'')', IQUEST(11), 1)
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
            GO TO 999
          ENDIF
*
*  **     Define the filename from client process name
*  **     Example: HCAL$DBMCOM:H__DBC_UR.DBMCOM
*
          FILECB(1:12)  = DIRECB
          FILECB(13:21) = DBCCM
          FILECB(22:)   = '.DBMCOM'
*         For time being map the whole common to disk
          CALL CC_SETUP ('CRCCOM', FILECB, IFRSCR, ILSTCR, 'M', IERR)
          IF (IERR.NE.0) THEN
            IRC        = -7
            IQUEST(11) = IERR
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDINIT : Err'//
     +      'or '',I10,'' in CC_SETUP'')', IQUEST(11), 1)
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
            GO TO 999
          ENDIF
          INSRCD = 1
        ENDIF
#endif
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
      ENDIF
#endif
*
* *** Construct the character option
*
      CHOPT  = ' '
      II     = 0
      DO 30 I = 1, NOPRZ
        IF (IOPRZ(I).NE.0) THEN
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
          IF (IOPPCD.NE.0) THEN
            IF (IACRZ(I).EQ.0) GO TO 30
          ENDIF
#endif
          II     = II + 1
          CHOPT(II:II) = COPRZ(I)
        ENDIF
   30 CONTINUE
      IOUTCD = 0
      IF (IOPTO.NE.0.OR.IOPTN.NE.0) IOUTCD = 1
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
      IF (IOPPCD.NE.0) IOPTN  = 0
#endif
      IOPTT = INDEX(CHOP,'T')
      IF (IOPTT.NE.0.OR.IOUTCD.NE.0.OR.IOPRZ(5).NE.0) THEN
        MXINCD  = 0
      ELSE
        CALL DATIME (IDATE, ITIME)
        CALL CDPKTM (IDATE, ITIME, MXINCD, IRC)
      ENDIF
*
*  ** Check whether the RZ file has already been initialized
*
      IF (LTOPCD.NE.0) THEN
        LBUPCD = LTOPCD
   40   LUN    = IQ(KOFUCD+LBUPCD+MUPLUN)
        NCH    = IQ(KOFUCD+LBUPCD+MUPNCH)
        CALL UHTOC (IQ(KOFUCD+LBUPCD+MUPNAM), 4, TOP1CT, NCH)
        IF (TOP1CT(1:NCH).EQ.TOPNCD.AND.LUN.EQ.LURZCD) THEN
          IRC    = -2
          LBUPCD = 0
#if defined(CERNLIB__DEBUG)
          CALL CDPRNT (LPRTCD, '(/,'' CDINIT : Top Directory '//TOPNCD//
     +         ' is already open on unit '',I4)', LURZCD, 1)
#endif
          GO TO 999
        ENDIF
        IF (TOP1CT(1:NCH).EQ.TOPNCD.OR.LUN.EQ.LURZCD) THEN
          IRC    = -3
          LBUPCD = 0
#if defined(CERNLIB__DEBUG)
          IARGCD(1) = LUN
          IARGCD(2) = LURZCD
          CALL CDPRNT (LPRTCD, '(/,'' CDINIT : Top Directory '//
     +         TOP1CT(1:NCH)//' open on unit '',I4,'' is in conflict'//
     +         ' with'',/,'' Requested Top Directory '//TOPNCD//' on '//
     +         'unit '',5X,I4)', IARGCD, 2)
#endif
          GO TO 999
        ENDIF
*
        LBFXCD = LBUPCD
        LBUPCD = LQ(KOFUCD+LBFXCD)
        IF (LBUPCD.NE.0)       GO TO 40
*
*  **   Create linear structure of the top-directories
*
        CALL MZBOOK (IDIVCD, LBUPCD, LBFXCD, 0, 'UPCD', NLUPCD, NSUPCD,
     +               NDUPCD, IOUPCD, -1)
      ELSE
        CALL MZBOOK (IDIVCD, LTOPCD, LTOPCD, 1, 'UPCD', NLUPCD, NSUPCD,
     +               NDUPCD, IOUPCD, -1)
        LBUPCD = LTOPCD
      ENDIF
*
*  ** Fill up the top directory bank with user data
*
      NTOPCD = NTOPCD + 1
      LUFZCD = 0
      LUBKCD = 0
      IF (IOUTCD.EQ.0)  IOPSCD = 0
      IQ(KOFUCD+LBUPCD+MUPLUN) = LURZCD
      IQ(KOFUCD+LBUPCD+MUPFLG) = IOUTCD
      IQ(KOFUCD+LBUPCD+MUPSRV) = IOPPCD
      IQ(KOFUCD+LBUPCD+MUPSHR) = IOPSCD
      IQ(KOFUCD+LBUPCD+MUPKIN) = MXINCD
      IQ(KOFUCD+LBUPCD+MUPJFL) = LUFZCD
      IQ(KOFUCD+LBUPCD+MUPBAK) = LUBKCD
      IQ(KOFUCD+LBUPCD+MUPNCH) = NCHRCD
      CALL UCTOH (TOPNCD, IQ(KOFUCD+LBUPCD+MUPNAM), 4, 16)
*
      IRC    = 0
#if defined(CERNLIB__P3CHILD)
      IF (IOPPCD.NE.0)  THEN
*
*  **   Request host to open the RZ file
*
        RNDBP3 = 'CDINIT'
        NWDBP3 = 8
        IWDBP3(1) = LURZCD
        CALL UCTOH  (TOPNCD, IWDBP3(2), 4, 16)
        IWDBP3(6) = NREC
        MSDBP3 = CHOP
        CALL UCTOH  (MSDBP3, IWDBP3(7), 4, 8)
        CALL CDCHLD
        IF (IQDBP3.NE.0) THEN
          CALL MZDROP (IDIVCD, LBUPCD, ' ')
          IRC        =-10
          IQUEST(11) = IQDBP3
#endif
#if (defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__DEBUG))
          CALL CDPRNT (LPRTCD, '(/,'' CDINIT : Host unable to open '//
     +         'directory '//TOPNCD//' on unit '',I4)', LURZCD, 1)
#endif
#if defined(CERNLIB__P3CHILD)
          GO TO 60
        ENDIF
      ENDIF
*
#endif
      IF (IOPTN.EQ.0)  THEN
*
*  **   Reopen the RZ-file on the mass storage for read and write
*
        IQUEST(10) = IQ10
        IQUEST(11) = IQ11
        CALL RZFILE (LURZCD, TOPNCD, CHOPT)
        IF(IQUEST(1).NE.0) THEN
           CALL RZEND  (TOPNCD)
           IQUEST(10) = IQ10
           IQUEST(11) = IQ11
           CALL RZFILE (LURZCD, TOPNCD, CHOPT)
        ENDIF
*
      ELSE
*
*  **   Create file on mass storage and create the 1st pass directories
*
        NSYS   = NOF2CK + NPAIR * 2
        CFOR   = CHFTCK(1:NSYS)
        DO I = 1, NSYS
          CTAGCK(I) = CHTGCK(I)
        ENDDO
        DO I = 1, NPAIR
          CFOR(NOF1CK+2*I-1:NOF1CK+2*I-1) = 'I'
          CFOR(NOF1CK+2*I  :NOF1CK+2*I  ) = 'I'
          CTAGCK(NOF1CK+2*I-1) = 'STR_VAL'//CALFCA(27+I)
          CTAGCK(NOF1CK+2*I)   = 'END_VAL'//CALFCA(27+I)
        ENDDO
*
*     New RZ format?
*
        IOPT7 = INDEX(CHOP,'7')
        IF(IOPT7.NE.0) CHOPT(II+1:II+1) = 'N'
        CALL RZMAKE (LURZCD, TOPNCD, NSYS, CFOR, CTAGCK, NRECS, CHOPT)
*
      ENDIF
*
      IF (IQUEST(1).NE.0)  THEN
        CALL MZDROP (IDIVCD, LBUPCD, ' ')
        CALL RZEND  (TOPNCD)
        IRC    = -2
#if defined(CERNLIB__DEBUG)
        CALL CDPRNT (LPRTCD, '(/,'' CDINIT : Top Directory '//TOPNCD//
     +       ' is already open on unit '',I4)', LURZ, 1)
#endif
      ELSE
*
        PAT1CT = '//'//TOPNCD
        LBFXCD = 0
        IF (IOPTN.EQ.0)  THEN
*
*   *     Get the version number
*
          CALL RZCDIR (PAT1CT, ' ')
          IF (IQUEST(1).EQ.0) THEN
            NKEYCK = IQUEST(7)
            NWKYCK = IQUEST(8)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            IF (NKEYCK.GT.0) THEN
              IPNT   = KOFSCD + LCDRCD + IKDRCD
              ITOPCD = IQ(IPNT+IDHKSN)
              NPARCD = IQ(IPNT+IDHPTR)
              IHFLCD = MOD(IQ(IPNT+IDHFLG)/64, 2)
              IERR   = 0
            ELSE
              IERR   = 1
            ENDIF
          ELSE
            IERR   = IQUEST(1)
          ENDIF
*
        ELSE
*
*   *     Create the version number
*
          CALL UOPTC (CHOP, 'A', IHFLCD)
          CALL VZERO (KEYVCK, NSYS)
          KEYVCK(IDHKSN) = NTOP
          KEYVCK(IDHPTR) = NPAIR
          KEYVCK(IDHFLG) = IHFLCD*64
          CALL CDBANK (IDIVCD, LBFXCD, LBFXCD, 2, 'UPCD', 0, 0, NSYS,
     +                 2, 0, IERR)
          IF (IERR.EQ.0) THEN
            CALL UCOPY (KEYVCK, IQ(KOFUCD+LBFXCD+1), NSYS)
            CALL RZOUT (IDIVCD, LBFXCD, KEYVCK, ICYCLE, 'S')
            IERR   = IQUEST(1)
            CALL MZDROP (IDIVCD, LBFXCD, ' ')
          ENDIF
          ITOPCD = NTOP
          NPARCD = NPAIR
          LBFXCD = 0
*
        ENDIF
*
        IF (IERR.NE.0) THEN
          CALL MZDROP (IDIVCD, LBUPCD, ' ')
          CALL RZEND  (TOPNCD)
          IRC    = IERR
          GO TO 60
        ENDIF
*
        IQ(KOFUCD+LBUPCD+MUPDIC) = ITOPCD
        IQ(KOFUCD+LBUPCD+MUPAIR) = NPARCD
        IQ(KOFUCD+LBUPCD+MUPHFL) = IHFLCD
*
*   *   Load the dictionary directory
*
        CALL CDUDIC (IRC)
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__ONLINE))
        IRC    = 0
#endif
        IF (IRC.NE.0) THEN
          CALL MZDROP (IDIVCD, LBUPCD, ' ')
          CALL RZEND  (TOPNCD)
          GO TO 60
        ENDIF
*
*   *   Link it to the list of directory names to be updated
*
        LREFCD(1) = LBADCD
   50   IF (LREFCD(1).NE.0) THEN
          CALL UHTOC (IQ(KOFUCD+LREFCD(1)+MFZTOP), 4, TOP1CT, 16)
          IF (TOPNCD.EQ.TOP1CT) THEN
            LQ(KOFUCD+LBUPCD-KLFZCD) = LREFCD(1)
          ELSE
            LREFCD(1) = LQ(KOFUCD+LREFCD(1))
            GO TO 50
          ENDIF
        ENDIF
*
      ENDIF
*
   60 LBUPCD = 0
*
*   * Reset top directory
*
      CALL CDCDIR (PAT1CT, ' ',IRC)
*                                                             END CDINIT
  999 CONTINUE
C ACP_data_retrieval_end
      END
