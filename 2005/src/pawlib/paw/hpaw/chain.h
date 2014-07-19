/*
 * $Id: chain.h,v 1.1.1.1 1996/03/01 11:39:14 mclareni Exp $
 *
 * $Log: chain.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:14  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.30.24  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   23/10/92*/
/***********************************************************************
 *                                                                     *
 * chain.h: declarations and definitions for the Ntuple chain.         *
 *                                                                     *
 ***********************************************************************/
#ifndef _chain_h
#define _chain_h

#if defined(CERNLIB_QX_SC)
#include "chain1.h"
#endif
#if defined(CERNLIB_IBM)
#include "chain2.h"
#endif
#if defined(CERNLIB_CRAY)
#include "chain3.h"
#endif

#define CHAIN_NAME  80
#define FILE_NAME   128

/* type of entries in EntryStruct */
#define CHAIN       1
#define FILE        2
#define LUN         3

/* type of operations (used in PCHEVT) */
#define SET         1
#define ADD         2


/* type declarations */
typedef struct _NtupleStruct    Ntuple;
typedef struct _EntryStruct     Entry;
typedef struct _ChainStruct     Chain;

struct _NtupleStruct {
   int              id;              /* id of Ntuple */
   char            *dir;             /* Ntuple directory */
   int              rows;            /* number of rows in Ntuple in chain */
};

struct _EntryStruct {
   int              type;            /* type of entry: CHAIN, FILE or LUN */
   char             name[FILE_NAME]; /* entry name */
   void            *widget;          /* widget associated with node */
   Chain           *chain;           /* pointer to Chain when type is chain */
   Entry           *link;            /* next entry */
};

struct _ChainStruct {
   char             name[CHAIN_NAME];   /* name of chain */
   void            *widget;             /* widget associated with chain */
   char            *path;               /* path to be appended to entries */
   int              no_ntuples;         /* number of Ntuples in chain */
   Ntuple          *ntuples;            /* array of Ntuples in chain */
   int              no_entries;         /* number of entries in chain */
   Entry           *entries;            /* list of entries in chain */
   Entry           *last_entry;         /* last item in entry list */
   Chain           *rtn_chain;          /* used during chain traversal */
   Entry           *rtn_entry;          /* used during chain traversal */
   Chain           *link;               /* next chain */
};


/* public chain functions */
extern Chain *find_chain(char *name);
extern void delete_chain_entry(char *chain_name, char *entry_name);


#endif  /* _chain_h */
