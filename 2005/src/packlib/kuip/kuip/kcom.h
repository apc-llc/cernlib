/*
 * $Id: kcom.h,v 1.1.1.1 1996/03/08 15:33:00 mclareni Exp $
 *
 * $Log: kcom.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:00  mclareni
 * Kuip
 *
 */
/* kcom.h: Fortran COMMON blocks */


#define  Kcparc           F77_BLOCK(kcparc,KCPARC)

EXTERN struct {
  char PARLST[512];             /* interface block for KUSER */
  char CLIST[80];
  char NOALIN[512];
  char COMAND[80];
  char CHLAST[512];
  char NONPOS[512];
} F77_COMMON(Kcparc);


#define  Kcutil           F77_BLOCK(kcutil,KCUTIL)

EXTERN struct {
  INTEGER NCMD;
  INTEGER IWD;
  INTEGER LUNFIL;
  INTEGER LPRMPT;
  LOGICAL TIMING;
  LOGICAL TRACE;
  INTEGER CALMOD;
  INTEGER NVADD;
  INTEGER IREPET;
  INTEGER IREFAC;
  INTEGER IBRAK;
  LOGICAL TIMALL;
  INTEGER LENTER;
  LOGICAL UNIQUE;
  INTEGER LENMUL;
  LOGICAL MULTFL;
  LOGICAL HISTOK;
  LOGICAL NOHIST;
  INTEGER LENMUM;
  LOGICAL FILCAS;
  LOGICAL MEXEFL;
} F77_COMMON(Kcutil);


#define  Kcvect           F77_BLOCK(kcvect,KCVECT)

EXTERN struct {
  INTEGER NUMVEC;               /* number of vectors stored */
  INTEGER TOTPAV;
  INTEGER GETPAV;
  LOGICAL TVECFL;
} F77_COMMON(Kcvect);


#define  Kcwork           F77_BLOCK(kcwork,KCWORK)

EXTERN struct {
  REAL VECTOR[100];             /* vector '?' */
} F77_COMMON(Kcwork);


#define Quest F77_BLOCK(quest,QUEST)

EXTERN struct {
  INTEGER DATA[100];
} F77_COMMON(Quest);

#define IQUEST(n) Quest.DATA[n-1]


#define Sikuip F77_BLOCK(sikuip,SIKUIP)

EXTERN struct {
  char CHSIGM[80];              /* command string passed to SIGMA */
} F77_COMMON(Sikuip);


