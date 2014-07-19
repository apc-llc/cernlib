*
* $Id: czoptd.inc,v 1.1.1.1 1996/03/08 15:44:18 mclareni Exp $
*
* $Log: czoptd.inc,v $
* Revision 1.1.1.1  1996/03/08 15:44:18  mclareni
* Cspack
*
*
#ifndef CERNLIB_CSPACK_CZOPTD_INC
#define CERNLIB_CSPACK_CZOPTD_INC
*
*
* czoptd.inc
*
#if !defined(CERNLIB_LINUX)
      DIMENSION      IOPT(26)
      EQUIVALENCE    (IOPT(1),IOPTA),(IOPT(2),IOPTB),(IOPT(3),IOPTC),
     +               (IOPT(4),IOPTD),(IOPT(5),IOPTE),(IOPT(6),IOPTF),
     +               (IOPT(7),IOPTG),(IOPT(8),IOPTH),(IOPT(9),IOPTI),
     +               (IOPT(10),IOPTJ),(IOPT(11),IOPTK),(IOPT(12),IOPTL),
     +               (IOPT(13),IOPTM),(IOPT(14),IOPTN),(IOPT(15),IOPTO),
     +               (IOPT(16),IOPTP),(IOPT(17),IOPTQ),(IOPT(18),IOPTR),
     +               (IOPT(19),IOPTS),(IOPT(20),IOPTT),(IOPT(21),IOPTU),
     +               (IOPT(22),IOPTV),(IOPT(23),IOPTW),(IOPT(24),IOPTX),
     +               (IOPT(25),IOPTY),(IOPT(26),IOPTZ)
      CHARACTER*26   ALFBET
      CHARACTER*(*)  CHOPT
      DATA           ALFBET/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
*
#endif
#if defined(CERNLIB_LINUX)
      DIMENSION      IOPT(26)
      EQUIVALENCE    (IOPT(1),IOPTA),(IOPT(2),IOPTB),(IOPT(3),IOPTC),
     +               (IOPT(4),IOPTD),(IOPT(5),IOPTE),(IOPT(6),IOPTF),
     +               (IOPT(7),IOPTG),(IOPT(8),IOPTH),(IOPT(9),IOPTI),
     +               (IOPT(10),IOPTJ),(IOPT(11),IOPTK),(IOPT(12),IOPTL),
     +               (IOPT(13),IOPTM),(IOPT(14),IOPTN),(IOPT(15),IOPTO),
     +               (IOPT(16),IOPTP),(IOPT(17),IOPTQ),(IOPT(18),IOPTR),
     +               (IOPT(19),IOPTS),(IOPT(20),IOPTT),(IOPT(21),IOPTU),
     +               (IOPT(22),IOPTV),(IOPT(23),IOPTW),(IOPT(24),IOPTX),
     +               (IOPT(25),IOPTY),(IOPT(26),IOPTZ)
      CHARACTER*26   ALFBET
      CHARACTER*(*)  CHOPT
      PARAMETER (ALFBET='ABCDEFGHIJKLMNOPQRSTUVWXYZ')
*
#endif

#endif
