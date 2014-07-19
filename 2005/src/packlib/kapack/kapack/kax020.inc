*
* $Id: kax020.inc,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kax020.inc,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#ifndef CERNLIB_KAPACK_KAX020_INC
#define CERNLIB_KAPACK_KAX020_INC
*
*
* kax020.inc
*
      COMMON /KAX020/ JOBID, LBLK, LEVELS, LREC, LUNERR, LUNKAF, LUNLOG,
     +                MAXKEY, MAXPAK, MODIFY, NBLKS, NFREE,
     +                NEWLUN, RETURN,
     +                MAJKEY(1:1+MAXNAM), MINKEY(1:1+MAXNAM),
     +                KLDEF(0:LMQ), KLMAX(0:LMQ),
     +                KROOT(0:LMQ), KUNIT(0:LMQ), KPROOT
      LOGICAL MODIFY, NEWLUN, RETURN

#endif
