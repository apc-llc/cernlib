*
* $Id: hdboptd.inc,v 1.1.1.1 1996/02/28 16:24:03 mclareni Exp $
*
* $Log: hdboptd.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:03  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_HDBOPTD_INC
#define CERNLIB_HEPDB_HDBOPTD_INC
*
*
* hdboptd.inc
*
      DIMENSION      IOPT(36)
      INTEGER        IOPT
      EQUIVALENCE    (IOPT(1),IOPTA),(IOPT(2),IOPTB),(IOPT(3),IOPTC),
     +               (IOPT(4),IOPTD),(IOPT(5),IOPTE),(IOPT(6),IOPTF),
     +               (IOPT(7),IOPTG),(IOPT(8),IOPTH),(IOPT(9),IOPTI),
     +               (IOPT(10),IOPTJ),(IOPT(11),IOPTK),(IOPT(12),IOPTL),
     +               (IOPT(13),IOPTM),(IOPT(14),IOPTN),(IOPT(15),IOPTO),
     +               (IOPT(16),IOPTP),(IOPT(17),IOPTQ),(IOPT(18),IOPTR),
     +               (IOPT(19),IOPTS),(IOPT(20),IOPTT),(IOPT(21),IOPTU),
     +               (IOPT(22),IOPTV),(IOPT(23),IOPTW),(IOPT(24),IOPTX),
     +               (IOPT(25),IOPTY),(IOPT(26),IOPTZ),(IOPT(27),IOPT1),
     +               (IOPT(28),IOPT2),(IOPT(29),IOPT3),(IOPT(30),IOPT4),
     +               (IOPT(31),IOPT5),(IOPT(32),IOPT6),(IOPT(33),IOPT7),
     +               (IOPT(31),IOPT5),(IOPT(32),IOPT6),(IOPT(33),IOPT7),
     +               (IOPT(34),IOPT8),(IOPT(35),IOPT9),(IOPT(36),IOPT0)
      CHARACTER*36   ALFNUM
      DATA           ALFNUM/'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890'/

#endif
