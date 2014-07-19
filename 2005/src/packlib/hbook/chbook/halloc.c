/*
 * $Id: halloc.c,v 1.3 1997/09/02 13:08:57 mclareni Exp $
 *
 * $Log: halloc.c,v $
 * Revision 1.3  1997/09/02 13:08:57  mclareni
 * WINNT correction
 *
 * Revision 1.2  1997/03/14 17:03:17  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:27:47  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
 * First import
 *
 */
#include "hbook/pilot.h"
/*CMZ :          18/10/95  18.01.49  by  Julian Bunn*/
/*-- Author :*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#if defined(CERNLIB_CRAY)
#include <fortran.h>
#endif
#if defined(CERNLIB_VAX)
#include <descrip.h>
#endif
 
#define MY_STDCALL
#if defined(CERNLIB_QX_SC)
#define   hbinit1     hbinit1_
#define   hbfree      hbfree_
#define   hbfreec     hbfreec_
#define   hballo1     hballo1_
#define   hbvm        hbvm_
#define   hbsmax      hbsmax_
#define   hbgmax      hbgmax_
#define   hbprnt      hbprnt_
#endif
#if defined(CERNLIB_CRAY)
#define   hbinit1     HBINIT1
#define   hbfree      HBFREE
#define   hbfreec     HBFREEC
#define   hballo1     HBALLO1
#define   hbvm        HBVM
#define   hbsmax      HBSMAX
#define   hbgmax      HBGMAX
#define   hbprnt      HBPRNT
#endif
#if defined(CERNLIB_WINNT) || defined(CERNLIB_MSSTDCALL)
#define   hbinit1     HBINIT1
#define   hbfree      HBFREE
#define   hbfreec     HBFREEC
#define   hballo1     HBALLO1
#define   hbvm        HBVM
#define   hbsmax      HBSMAX
#define   hbgmax      HBGMAX
#define   hbprnt      HBPRNT
#define   MY_STDCALL  __stdcall
#endif
#if defined(CERNLIB_IBM)
#pragma linkage(hbinit1,FORTRAN)
#pragma linkage(hbfree,FORTRAN)
#pragma linkage(hbfreec,FORTRAN)
#pragma linkage(hballo1,FORTRAN)
#pragma linkage(hbvm,FORTRAN)
#pragma linkage(hbsmax,FORTRAN)
#pragma linkage(hbgmax,FORTRAN)
#pragma linkage(hbprnt,FORTRAN)
#endif
 
#ifndef NULL
#  define NULL 0
#endif
 
#define False       0
#define True        1
 
#define MAX_MALLOC  10485760    /* 10 MB default maximum */
#define UMAX_MALLOC 128         /* 128 MB user definable maximum */
 
#define REAL        1
#define INTEGER     2
#define UINTEGER    3
#define BOOLEAN     4
#define CHAR        5
 
#ifdef CRAY
typedef long     Boolean;
#else
typedef char     Boolean;
#endif
 
 
/* type declarations */
typedef struct _NtupleBuffer  NtupleBuffer;
struct _NtupleBuffer {
   int             id;
   char            path[128];
   char            var[36];
   char            block[16];
   int             type;
   int             size;
   int             ifirst;
   int             nelem;
   Boolean         in_use;
   int             no_times_used;
   time_t          last_used;
   void           *data;
   NtupleBuffer   *link;
};
 
 
/* root and last in NtupleBuffer list */
static NtupleBuffer  *ntbuf_root = NULL;
static NtupleBuffer  *ntbuf_last = NULL;
 
/* the amount of currently malloced space */
static unsigned int   malloc_used = 0;
static unsigned int   malloc_max  = MAX_MALLOC;  /* build in limit */
static unsigned int   malloc_umax = MAX_MALLOC;  /* user set limit */
 
/* the amount of space accessed during the programs' lifetime (in words) */
static float          malloc_total = 0.0;
 
 
/***********************************************************************
 *                                                                     *
 *   Free the least used, oldest, not in_use Ntuple buffer (column).   *
 *                                                                     *
 ***********************************************************************/
static int free_column(void)
{
   NtupleBuffer  *tmp, *tmp1, *t1, *t2;
 
   /* find least used, oldest, not in_use, with allocated data buffer */
   t1   = NULL;
   tmp1 = NULL;
   tmp  = ntbuf_root;
   while (tmp) {
      if (!tmp->in_use && tmp->data) {
	 if (t1) {
	    if (tmp->no_times_used < t1->no_times_used) {
	       t1 = tmp;
	       t2 = tmp1;
	    }
	    if (tmp->no_times_used == t1->no_times_used &&
		tmp->last_used     <  t1->last_used) {
	       t1 = tmp;
	       t2 = tmp1;
	    }
	 } else {
	    t1 = tmp;
	    t2 = tmp1;
	 }
      }
      tmp1 = tmp;
      tmp  = tmp->link;
   }
 
   if (t1) {
      /* remove t1 from list */
      if (t2) {
	 t2->link = t1->link;
         if (ntbuf_last == t1) ntbuf_last = t2;
      } else {
	 ntbuf_root = t1->link;
         if (ntbuf_last == t1) ntbuf_last = NULL;
      }
 
      malloc_used -= t1->size*t1->nelem + sizeof(NtupleBuffer);
 
      /* recuperate space */
      /* printf("free_column: Deleted column %s from memory\n", t1->var); */
      free(t1->data);
      free(t1);
 
      return 0;
   }
 
   /* cannot free any space */
   return -1;
}
 
/***********************************************************************
 *                                                                     *
 *   Free all Ntuple buffers.                                          *
 *                                                                     *
 ***********************************************************************/
static void free_all(void)
{
   NtupleBuffer  *t1, *t2;
 
   t1 = ntbuf_root;
   while (t1) {
      t2 = t1->link;
      free(t1->data);
      free(t1);
      t1 = t2;
   }
   ntbuf_root  = NULL;
   ntbuf_last  = NULL;
   malloc_used = 0;
   malloc_max  = malloc_umax;
}
 
/***********************************************************************
 *                                                                     *
 *   Free all Ntuple buffers for Ntuples in file "path".               *
 *                                                                     *
 ***********************************************************************/
static void free_lun(char *path)
{
   NtupleBuffer  *t1, *t2, *tmp;
   int            lp;
 
   lp = strlen(path);
 
   t1 = ntbuf_root;
   t2 = NULL;
   while (t1) {
      if (!strncmp(t1->path, path, lp)) {
         if (t2) {
            t2->link = t1->link;
            if (ntbuf_last == t1) ntbuf_last = t2;
         } else {
            ntbuf_root = t1->link;
            if (ntbuf_last == t1) ntbuf_last = NULL;
         }
 
         tmp = t1->link;
 
         malloc_used -= t1->size*t1->nelem + sizeof(NtupleBuffer);
 
         free(t1->data);
         free(t1);
 
         t1 = tmp;
      } else {
         t2 = t1;
         t1 = t1->link;
      }
   }
}
 
/***********************************************************************
 *                                                                     *
 *   Free all temporary Ntuple buffers. Temporary buffers are          *
 *   identified by variable names starting with an _. They are used to *
 *   store intermediate results during Ntuple plot and project         *
 *   commands. Temporary buffers are always deleted by hbinit1.        *
 *                                                                     *
 ***********************************************************************/
static void free_tmp(void)
{
   NtupleBuffer  *t1, *t2, *tmp;
 
   t1 = ntbuf_root;
   t2 = NULL;
   while (t1) {
      if (t1->var[0] == '_') {
         if (t2) {
            t2->link = t1->link;
            if (ntbuf_last == t1) ntbuf_last = t2;
         } else {
            ntbuf_root = t1->link;
            if (ntbuf_last == t1) ntbuf_last = NULL;
         }
 
         tmp = t1->link;
 
         malloc_used -= t1->size*t1->nelem + sizeof(NtupleBuffer);
 
         free(t1->data);
         free(t1);
 
         t1 = tmp;
      } else {
         t2 = t1;
         t1 = t1->link;
      }
   }
}
 
/***********************************************************************
 *                                                                     *
 *   Free all in_use Ntuple buffers. Called by hbfree with lun<0.      *
 *                                                                     *
 ***********************************************************************/
static void free_in_use(void)
{
   NtupleBuffer  *t1, *t2, *tmp;
 
   t1 = ntbuf_root;
   t2 = NULL;
   while (t1) {
      if (t1->in_use && t1->no_times_used <= 1) {
         if (t2) {
            t2->link = t1->link;
            if (ntbuf_last == t1) ntbuf_last = t2;
         } else {
            ntbuf_root = t1->link;
            if (ntbuf_last == t1) ntbuf_last = NULL;
         }
 
         tmp = t1->link;
 
         malloc_used -= t1->size*t1->nelem + sizeof(NtupleBuffer);
 
         free(t1->data);
         free(t1);
 
         t1 = tmp;
      } else {
         t2 = t1;
         t1 = t1->link;
      }
   }
}
 
/***********************************************************************
 *                                                                     *
 *   Free all Ntuple buffers or all buffers belonging to Ntuples in    *
 *   the same unit.                                                    *
 *   If lun<0 free all Ntuple buffers that are in use (do this from an *
 *   exception handler in case the filling of the buffers was          *
 *   interrupted).                                                     *
 *                                                                     *
 ***********************************************************************/
void  MY_STDCALL hbfree(int *lun)
{
   char    path[15];
 
   if (*lun == 0)
      free_all();
   else if (*lun < 0)
      free_in_use();
   else {
      sprintf(path, "//LUN%d", *lun);
      free_lun(path);
      strcpy(path, "//PAWC");  /* <--- add these two lines */
      free_lun(path);          /* <---                     */
   }
}
 
/***********************************************************************
 *                                                                     *
 *   Free all Ntuple buffers belonging to Ntuples in the chain CHAIN.  *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
void hbfreec( struct dsc$descriptor_s *dsc_chain1, int *lc)
{
   char *chain1  = dsc_chain1->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
void hbfreec(_fcd fcd_chain1, int *lc)
{
   char *chain1  = _fcdtocp( fcd_chain1 );
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))
#ifndef CERNLIB_MSSTDCALL
void MY_STDCALL hbfreec(char *chain1, int *lc)
#else
void MY_STDCALL hbfreec(char *chain1, int len_chain1, int *lc)
#endif
{
#endif
   char    chain[80];
 
   strncpy(chain,  chain1,  *lc);  chain[*lc]  = '\0';
 
   free_lun(chain);
}
 
/***********************************************************************
 *                                                                     *
 *   Report the total number of words accessed during the programs'    *
 *   lifetime.                                                         *
 *                                                                     *
 ***********************************************************************/
void MY_STDCALL hbvm(float *words)
{
   *words = malloc_total;
}
 
/***********************************************************************
 *                                                                     *
 *   Change the maximum amount of memory that may be malloced.         *
 *                                                                     *
 ***********************************************************************/
void MY_STDCALL hbsmax(int *mbytes)
{
   int mb = *mbytes;
 
   if (mb >= 0) {
      if (mb > UMAX_MALLOC) mb = UMAX_MALLOC;
 
      malloc_umax = mb*1024*1024;
      malloc_max  = malloc_umax;
   }
}
 
/***********************************************************************
 *                                                                     *
 *   Get the maximum amount of memory that may be malloced.            *
 *                                                                     *
 ***********************************************************************/
void MY_STDCALL hbgmax(int *mbytes)
{
   *mbytes = malloc_umax/(1024*1024);
}
 
/***********************************************************************
 *                                                                     *
 *   Print buffer statistics.                                          *
 *                                                                     *
 ***********************************************************************/
void MY_STDCALL hbprnt(int *flag)
{
   NtupleBuffer   *ntbuf;
   char            type;
 
   if (*flag == 1) {
      printf("\n  Dynamic Ntuple Memory Buffer Statistics (in bytes):\n\n");
      printf("  Upper Limit        Used               Free\n");
      printf("  %-10d         %-10d         %-10d\n\n", malloc_max,
             malloc_used, malloc_max - malloc_used);
 
   } else {
 
      ntbuf = ntbuf_root;
      printf("\n  Variable         Lun     Ntuple     Type     Usage");
      printf("         Size\n");
      printf("----------------------------------------------------");
      printf("-------------\n");
      if (!ntbuf) {
         printf("  No variables in memory.\n\n");
         return;
      }
 
      while (ntbuf) {
         switch (ntbuf->type) {
            case REAL:
               type = 'R';
               break;
            case INTEGER:
               type = 'I';
               break;
            case UINTEGER:
               type = 'U';
               break;
            case BOOLEAN:
               type = 'L';
               break;
            case CHAR:
               type = 'C';
               break;
         }
         printf("%10s  %10s   %8d       %c      %5d    %9d\n",
                ntbuf->var, ntbuf->path, ntbuf->id, type,
                ntbuf->no_times_used, ntbuf->size*ntbuf->nelem);
 
         ntbuf = ntbuf->link;
         if (!ntbuf)
            printf("\n");
      }
   }
}
 
/***********************************************************************
 *                                                                     *
 *   Initialize the Ntuple buffer manager, i.e. declare all buffers    *
 *   unused (but don't free them because they will be used again).     *
 *                                                                     *
 ***********************************************************************/
void MY_STDCALL hbinit1(int *tfree)
{
   NtupleBuffer   *tmp;
 
   /* clear in_use flag of all buffers */
   tmp = ntbuf_root;
   while (tmp) {
      tmp->in_use = False;
      tmp = tmp->link;
   }
 
   /* free space occupied by tmp buffers */
   if (*tfree == 1)
      free_tmp();
}
 
/***********************************************************************
 *                                                                     *
 *   Allocate space for Ntuple column of size*nelem bytes.             *
 *   If not enough space call free_column to free the least used,      *
 *   oldest not in_use buffer.                                         *
 *                                                                     *
 ***********************************************************************/
static NtupleBuffer *hballo2(int id, char *path, char *var, char *block,
                             int type, int size, int ifirst, int nelem)
{
   NtupleBuffer   *ntbuf, *tmp;
 
   /* see if buffer is already allocated */
   tmp = ntbuf_root;
   while (tmp) {
      /*
       * For the time being the variable name must be unique in an Ntuple
       * so no need to test on the block name.
       * if (tmp->id == id && !strcmp(tmp->path, path) &&
       *     !strcmp(tmp->var, var) && !strcmp(tmp->block, block)) {
       */
      if (tmp->id == id && !strcmp(tmp->path,path) && !strcmp(tmp->var,var)) {
	 ntbuf = tmp;
	 break;
      }
      tmp = tmp->link;
   }
 
   /* if not allocate it */
   if (!tmp) {
      /* is there enough space left? */
again:
      while (malloc_used + size*nelem + sizeof(NtupleBuffer) > malloc_max) {
	 if (free_column() == -1)
	    return NULL;
         goto again;
      }
 
      ntbuf = (NtupleBuffer *) calloc(1, sizeof(NtupleBuffer));
      if (!ntbuf) {
	 if (free_column() == -1)
	    return NULL;
	 goto again;
      }
 
      strcpy(ntbuf->path,  path);
      strcpy(ntbuf->var,   var);
      strcpy(ntbuf->block, block);
      ntbuf->id     = id;
      ntbuf->type   = type;
      ntbuf->size   = size;
      ntbuf->ifirst = ifirst;
      ntbuf->nelem  = nelem;
      ntbuf->data   = malloc(size*nelem);
 
      if (!ntbuf->data) {
	 free(ntbuf);
	 if (free_column() == -1)
	    return NULL;
	 goto again;
      }
 
      if (!ntbuf_root) {
	 ntbuf_root = ntbuf;
	 ntbuf_last = ntbuf;
      } else {
	 ntbuf_last->link = ntbuf;
	 ntbuf_last       = ntbuf;
      }
 
      malloc_used  += size*nelem + sizeof(NtupleBuffer);
 
   } else if (nelem > ntbuf->nelem || ifirst != ntbuf->ifirst ||
              strlen(path) == 0) {
 
      void *data;
 
      /*
       * Buffer is already allocated but too small or contains the
       * wrong range of elements or memory resident Ntuple.
       * Turn on in_use flag so when columns have to be freed to get
       * more space this column is not freed, and reset the no_times_use
       * counter so the buffer will be filled again.
       */
      ntbuf->no_times_used = 0;
      ntbuf->in_use = True;
 
again1:
      while (malloc_used + size*(nelem-ntbuf->nelem) > malloc_max) {
	 if (free_column() == -1) {
            ntbuf->in_use = False;   /* buffer is not used so may be freed */
	    return NULL;
         }
         goto again1;
      }
 
      ntbuf->ifirst = ifirst;
      ntbuf->nelem  = nelem;
      data = realloc(ntbuf->data, size*nelem);
 
      if (!data) {
	 if (free_column() == -1) {
            ntbuf->in_use = False;   /* buffer is not used so may be freed */
	    return NULL;
         }
	 goto again1;
      }
 
      ntbuf->data = data;
 
   }
 
   ntbuf->no_times_used++;
   ntbuf->in_use    = True;
   ntbuf->last_used = time(NULL);
 
   malloc_total += (float)(size*nelem + sizeof(NtupleBuffer))/4.0;
 
   return ntbuf;
}
 
/***********************************************************************
 *                                                                     *
 *   Interface routine to hballo2 which allocates the buffer.          *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
void hballo1(int *id, struct dsc$descriptor_s *dsc_path1, int *lp,
             struct dsc$descriptor_s *dsc_var1, int *lv,
             struct dsc$descriptor_s *dsc_block1, int *lb, int *type,
             int *size, int *ifirst, int *nelem, long *buf, int *nuse)
{
   char *path1  = dsc_path1->dsc$a_pointer;
   char *var1   = dsc_var1->dsc$a_pointer;
   char *block1 = dsc_block1->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
void hballo1(int *id, _fcd fcd_path1, int *lp, _fcd fcd_var1, int *lv,
             _fcd fcd_block1, int *lb, int *type,
	     int *size, int *ifirst, int *nelem, long *buf, int *nuse)
{
   char *path1  = _fcdtocp( fcd_path1 );
   char *var1   = _fcdtocp( fcd_var1 );
   char *block1 = _fcdtocp( fcd_block1 );
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))
#ifndef CERNLIB_MSSTDCALL
void MY_STDCALL hballo1(int *id, char *path1, int *lp, char *var1, int *lv,
             char *block1, int *lb, int *type,
	     int *size, int *ifirst, int *nelem, long *buf, int *nuse)
#else
void MY_STDCALL hballo1(int *id, char *path1, int len_path1, int *lp, 
                                 char *var1,  int len_var1,  int *lv,
                                 char *block1,int len_block1, 
                        int *lb, int *type, int *size, int *ifirst, 
                        int *nelem, long *buf, int *nuse)
#endif
{
#endif
   char            path[128], var[36], block[16];
   unsigned long   tmp;
   NtupleBuffer   *ntbuf;
 
   strncpy(path,  path1,  *lp);  path[*lp]  = '\0';
   strncpy(var,   var1,   *lv);  var[*lv]   = '\0';
   strncpy(block, block1, *lb);  block[*lb] = '\0';
 
   ntbuf = hballo2(*id, path, var, block, *type, *size, *ifirst, *nelem);
 
   if (!ntbuf) {
      *nuse = 0;
      *buf  = 0;
   } else {
      *nuse = ntbuf->no_times_used;
      tmp   = (unsigned long) ntbuf->data;
      *buf  = (long) (tmp >> 2);
   }
}
