/*
 * $Id: chain.c,v 1.2 1999/09/15 13:01:13 mclareni Exp $
 *
 * $Log: chain.c,v $
 * Revision 1.2  1999/09/15 13:01:13  mclareni
 * Remove strcasecmp redefinition
 *
 * Revision 1.1.1.1  1996/03/01 11:38:58  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.07/19 30/11/95  14.05.51  by  Unknown*/
/*-- Author :    Fons Rademakers   23/10/92*/
/***********************************************************************
 *                                                                     *
 * chain.c: Ntuple chain routines.                                     *
 *                                                                     *
 ***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(CERNLIB_CRAY)
#include <fortran.h>
#endif
#if defined(CERNLIB_VAX)
#include <descrip.h>
#endif

#include "hpaw/chain.h"

#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#endif

extern void ku_exec(char *);

/* pointers to the chain and entry currently being traversed */
static Chain  *current_chain = NULL;
static Entry  *current_entry = NULL;
static char   *current_path  = NULL;
static int     current_depth = 0;

/* root and last in Chain list */
static Chain  *chain_root = NULL;
static Chain  *chain_last = NULL;

/* forward declaration of local functions */
static void delete_a_chain(char *name);


/***********************************************************************
 *                                                                     *
 *   Find a chain "name" and return a pointer to the chain.            *
 *                                                                     *
 ***********************************************************************/
Chain *find_chain(char *name)
{
   Chain   *chain;

   chain = chain_root;
   while (chain) {
      if (!strcasecmp(chain->name, name)) return chain;
      chain = chain->link;
   }

   return (Chain *) NULL;
}

/***********************************************************************
 *                                                                     *
 *   Find in a chain the index of an Ntuple with a given id.           *
 *                                                                     *
 ***********************************************************************/
static int find_ntuple(Chain *chain, int id, char *dir)
{
    int i;

    for (i = 0; i < chain->no_ntuples; i++)
       if (chain->ntuples[i].id == id &&
           !strcasecmp(chain->ntuples[i].dir, dir))
          return i;

    return -1;
}

/***********************************************************************
 *                                                                     *
 *   Return the parent of a chain.                                     *
 *                                                                     *
 ***********************************************************************/
static Chain *chain_parent(Chain *chain)
{
   Chain    *parent;
   Entry    *entry;

   if (chain) {

      parent = chain_root;
      while (parent) {

         entry = parent->entries;
         while (entry) {

            if (entry->type == CHAIN && !strcasecmp(entry->name, chain->name))
               return parent;

            entry = entry->link;
         }

         parent = parent->link;
      }
   }
   return (Chain *) NULL;
}

/***********************************************************************
 *                                                                     *
 *   Create a new chain and append it to the chain list.               *
 *                                                                     *
 ***********************************************************************/
static Chain *create_chain(char *name)
{
   Chain  *chain;

   /* allocate space for new chain and initialize */
   chain = calloc(1, sizeof(Chain));
   if (!chain) return (Chain *) NULL;

   strcpy(chain->name, name);

   /* add to the end of the chain list */
   if (!chain_root) {
      chain_root = chain;
      chain_last = chain;
   } else {
      chain_last->link = chain;
      chain_last       = chain;
   }

   return chain;
}

#if 0
/***********************************************************************
 *                                                                     *
 *   Find an entry in a chain and return a pointer to the entry.       *
 *                                                                     *
 ***********************************************************************/
static Entry *find_entry(Chain *chain, char *name)
{
   Entry   *entry;

   entry = chain->entries;
   while (entry) {
      if (!strcasecmp(entry->name, name)) return entry;
      entry = entry->link;
   }

   return (Entry *) NULL;
}
#endif

/***********************************************************************
 *                                                                     *
 *   Delete all Ntuple arrays from this chain upwards.                 *
 *   This is necessary because after deleting or adding a chain or     *
 *   chain entry the row count for the Ntuples is not valid anymore.   *
 *                                                                     *
 ***********************************************************************/
static void delete_ntuple_array(Chain *chain)
{
   Chain   *chn;
   int      i;

   /* delete Ntuple array from the chain */
   if (chain->no_ntuples) {
      for (i = 0; i < chain->no_ntuples; i++)
         free(chain->ntuples[i].dir);
      free(chain->ntuples);
      chain->no_ntuples = 0;
   }

   if ((chn = chain_parent(chain)))
      delete_ntuple_array(chn);
}

/***********************************************************************
 *                                                                     *
 *   Create a new entry and append it to the entry list of the chain.  *
 *                                                                     *
 ***********************************************************************/
static Entry *add_entry(Chain *chain, char *name, int type)
{
   Chain   *chn;
   Entry   *entry;

   /* check for recursive chain definition */
   if (type == CHAIN) {
      chn = find_chain(name);
      if (chn == chain) {
         printf("Circular chain definition, cannot add entry %s.\n", name);
         return (Entry *) NULL;
      }
   }

   /* allocate space for new entry and initialize */
   entry = calloc(1, sizeof(Entry));
   if (!entry) return (Entry *) NULL;

   entry->type = type;
   strcpy(entry->name, name);
   if (type == CHAIN) entry->chain = chn;
   chain->no_entries++;

   /* add to the end of the entry list */
   if (!chain->entries) {
      chain->entries    = entry;
      chain->last_entry = entry;
   } else {
      chain->last_entry->link = entry;
      chain->last_entry       = entry;
   }

   return entry;
}

/***********************************************************************
 *                                                                     *
 *   Add an entry to a chain. If the chain does not exist create it.   *
 *                                                                     *
 ***********************************************************************/
static int add_chain_entry(char *name, char *entry_name, int type)
{
   Chain   *chain;
   Entry   *entry;

   /* find or create chain */
   chain = find_chain(name);
   if (!chain) {
      chain = create_chain(name);
      if (!chain) {
         printf("Cannot create chain structure for %s.\n", name);
         return 0;
      }
   }

   entry = add_entry(chain, entry_name, type);
   if (!entry)
      return 0;
   else {
      /* delete all Ntuple arrays from this chain upwards */
      delete_ntuple_array(chain);
      return 1;
   }
}

/***********************************************************************
 *                                                                     *
 *   Delete an entry from a chain.                                     *
 *                                                                     *
 ***********************************************************************/
void delete_chain_entry(char *name, char *entry_name)
{
   Chain   *chain;
   Entry   *e1, *e2;

   /* find chain */
   chain = find_chain(name);
   if (!chain) {
      printf("Chain %s does not exist.\n", name);
      return;
   }

   /* find and delete entry from chain */
   e1 = chain->entries;
   e2 = NULL;

   while (e1) {
      if (!strcasecmp(e1->name, entry_name)) {
         if (e2) {
            e2->link = e1->link;
            if (chain->last_entry == e1) chain->last_entry = e2;
         } else {
            chain->entries = e1->link;
            if (chain->last_entry == e1) chain->last_entry = NULL;
         }
         free(e1);
         chain->no_entries--;

         /* delete all Ntuple arrays from this chain upwards */
         delete_ntuple_array(chain);

         if (chain->no_entries == 0)
            delete_a_chain(chain->name);

         return;
      }
      e2 = e1;
      e1 = e1->link;
   }

   printf("Entry %s not found.\n", entry_name);
}

/***********************************************************************
 *                                                                     *
 *   Delete all refrences to a chain.                                  *
 *                                                                     *
 ***********************************************************************/
static void delete_chain_references(Chain *chain)
{
   Chain   *chn;
   Entry   *e1, *e2;

   chn = chain_root;
   while (chn) {
      if (chn != chain) {
         e1 = chn->entries;
         e2 = NULL;
         while (e1) {
            if (e1->type == CHAIN && !strcasecmp(e1->name, chain->name)) {
               if (e2) {
                  e2->link = e1->link;
                  if (chn->last_entry == e1) chn->last_entry = e2;
               } else {
                  chn->entries = e1->link;
                  if (chn->last_entry == e1) chn->last_entry = NULL;
               }
               free(e1);
               chn->no_entries--;

               /* delete all Ntuple arrays from this chain upwards */
               delete_ntuple_array(chn);

               if (chn->no_entries == 0) {
                  delete_a_chain(chn->name);
                  break;
               }

               if (e2)
                  e1 = e2->link;
               else
                  e1 = chn->entries;

            } else {
               e2 = e1;
               e1 = e1->link;
            }
         }
      }
      chn = chn->link;
   }
}

/***********************************************************************
 *                                                                     *
 *   Delete a chain.                                                   *
 *                                                                     *
 ***********************************************************************/
static void delete_a_chain(char *name)
{
   Chain   *chain1, *chain2;
   Entry   *e1, *e2;
   int      i;

   /* find chain */
   chain1 = chain_root;
   chain2 = NULL;
   while (chain1) {
      if (!strcasecmp(chain1->name, name)) goto out;
      chain2 = chain1;
      chain1 = chain1->link;
   }

   printf("Chain %s does not exist.\n", name);
   return;

out:
   /* delete all entries from the chain */
   e1 = chain1->entries;

   while (e1) {
      e2 = e1->link;
      free(e1);
      e1 = e2;
   }
   chain1->entries = NULL;

   free(chain1->path);

   /* delete all Ntuple arrays from this chain upwards */
   delete_ntuple_array(chain1);

   /* delete all references to chain1 */
   delete_chain_references(chain1);

   /* finally delete the chain itself */
   if (chain2) {
      chain2->link = chain1->link;
      if (chain_last == chain1) chain_last = chain2;
   } else {
      chain_root = chain1->link;
      if (chain_last == chain1) chain_last = NULL;
   }
   free(chain1);
}

/***********************************************************************
 *                                                                     *
 *   Delete all chains.                                                *
 *                                                                     *
 ***********************************************************************/
static void delete_all_chains(void)
{
   Chain   *chn1, *chn2;
   Entry   *e1, *e2;
   int      i;

   chn1 = chain_root;
   while (chn1) {
      /* delete all entries from chain */
      e1 = chn1->entries;
      while (e1) {
         e2 = e1->link;
         free(e1);
         e1 = e2;
      }
      chn2 = chn1->link;

      /* delete Ntuple array from the chain */
      for (i = 0; i < chn1->no_ntuples; i++)
         free(chn1->ntuples[i].dir);
      free(chn1->ntuples);

      free(chn1->path);

      free(chn1);
      chn1 = chn2;
   }
   chain_root = NULL;
   chain_last = NULL;
}

/***********************************************************************
 *                                                                     *
 *   Check if a chain is a toplevel chain, i.e. the chain is not       *
 *   referenced in any other chain entry list.                         *
 *                                                                     *
 ***********************************************************************/
static int toplevel_chain(Chain *chain)
{
   if (chain_parent(chain))
      return 0;
   else
      return 1;
}

/***********************************************************************
 *                                                                     *
 *   Get the path active for this chain. If no path set for this chain *
 *   check for path up the tree. Return path or NULL if no path set    *
 *   in the tree.                                                      *
 *                                                                     *
 ***********************************************************************/
static char *get_path(Chain *chain)
{
   do {
      if (chain->path)
         return chain->path;
   } while ((chain = chain_parent(chain)));

   return NULL;
}

/***********************************************************************
 *                                                                     *
 *   Print list of all defined chains.                                 *
 *                                                                     *
 ***********************************************************************/
static void print_chain_entries(char *chain_name)
{
   Chain   *chain;
   Entry   *entry;
   char     form[20];
   int      i   = 0;
   int      mxl = 0;

   /* find chain */
   chain = find_chain(chain_name);
   if (!chain) {
      printf("Chain %s does not exist.\n", chain_name);
      return;
   }

   entry = chain->entries;
   while (entry) {
      mxl = max(strlen(entry->name), mxl);
      entry = entry->link;
   }
   sprintf(form, "  %%-%ds    ", mxl);

   printf("\n");
   entry = chain->entries;
   while (entry) {
      i++;
      printf(form, entry->name);
      if (i == 5) {
         printf("\n");
         i = 0;
      }
      entry = entry->link;
   }
   if (i) printf("\n");
}

/***********************************************************************
 *                                                                     *
 *   Print list of all defined chains.                                 *
 *                                                                     *
 ***********************************************************************/
static void print_chains(void)
{
   Chain   *chain;
   char     form[20];
   int      i   = 0;
   int      mxl = 0;

   if (!chain_root) {
      printf("No chains defined.\n");
      return;
   }

   chain = chain_root;
   while (chain) {
      mxl = max(strlen(chain->name), mxl);
      chain = chain->link;
   }
   sprintf(form, "  %%-%ds    ", mxl);

   printf("\n");
   chain = chain_root;
   while (chain) {
      i++;
      printf(form, chain->name);
      if (i == 5) {
         printf("\n");
         i = 0;
      }
      chain = chain->link;
   }
   if (i) printf("\n");
}

/***********************************************************************
 *                                                                     *
 *   Add depth times 3 spaces at the beginning of a line.              *
 *                                                                     *
 ***********************************************************************/
static void add_spaces(int depth)
{
   int   i;

   for (i = 0; i < depth; i++) {
      if (i == 0) printf("  ");
      printf("   ");
   }
}

/***********************************************************************
 *                                                                     *
 *   Print chain tree.                                                 *
 *                                                                     *
 ***********************************************************************/
static void print_chain_tree(char *chain_name, int depth)
{
   Chain  *chain;
   Entry  *entry;
   int     spc  = 0;
   int     newl = 0;

   chain = find_chain(chain_name);
   if (!chain) return;

   if (!depth) {
      printf("\n");
      printf("  %s\n", chain->name);
      depth++;
      add_spaces(depth);
   }

   entry = chain->entries;
   while (entry) {
      if (spc) {
         add_spaces(depth);
         spc = 0;
      }
      switch (entry->type) {
         case CHAIN:
            printf("%s\n", entry->name);
            depth++;
            add_spaces(depth);
            print_chain_tree(entry->name, depth);
            depth--;
            spc++;
            break;
         case LUN:
         case FILE:
            newl++;
            printf("%s   ", entry->name);
            break;
      }
      entry = entry->link;
   }
   if (newl) printf("\n");
}

/***********************************************************************
 *                                                                     *
 *   Init chain traversal be giving the name of the chain from where   *
 *   the traversal should start.                                       *
 *                                                                     *
 ***********************************************************************/
static void init_chain_traversal(char *chain_name)
{
   Chain    *chain;

   chain = find_chain(chain_name);
   if (!chain) {
      current_chain = (Chain *) NULL;
      printf("Chain %s does not exist.\n", chain_name);
   } else {
      chain->rtn_chain = (Chain *)NULL;
      chain->rtn_entry = (Entry *)NULL;
      current_chain = chain;
      current_path  = get_path(current_chain);
   }
   current_entry = (Entry *) NULL;
   current_depth = 0;
}

/***********************************************************************
 *                                                                     *
 *   Traverse the chain structure and hand back the next entry name.   *
 *   Chain entries are followed downwards. At the end of the tree the  *
 *   return value is 0. Space for entry_name must have been reserved   *
 *   by the calling routine.                                           *
 *                                                                     *
 ***********************************************************************/
static int next_chain_entry(char *entry_name, int *type, int *depth)
{
   Chain  *chain;
   Entry  *entry;

   if (!current_chain)
      return 0;

next:
   if (!current_entry)
      entry = current_chain->entries;
   else
      entry = current_entry->link;

   /* if NULL entry return to parent chain or end of chain traversal */
   while (!entry) {
      if (!current_chain->rtn_chain) {
         return 0;
      } else {
         entry = current_chain->rtn_entry->link;
         current_chain = current_chain->rtn_chain;
         current_path  = get_path(current_chain);
         current_depth--;
      }
   }

   if (entry->type == CHAIN) {

      /* if necessary resolve reference to chain */
      if (!entry->chain) {
         entry->chain = find_chain(entry->name);
         if (!entry->chain) {
            printf("Warning: Chain %s not defined.\n", entry->name);
            current_entry = entry;
            goto next;
         }
      }

      /* put return addresses */
      entry->chain->rtn_chain = current_chain;
      entry->chain->rtn_entry = entry;

      current_chain = entry->chain;
      current_path  = get_path(current_chain);
      current_entry = (Entry *) NULL;
      current_depth++;
   } else {
      current_entry = entry;
   }

   *depth = current_depth;
   *type  = entry->type;
   strcpy(entry_name, entry->name);

   return 1;
}

/***********************************************************************
 *                                                                     *
 *   Returns 1 if NAME is a chain else 0.                              *
 *                                                                     *
 ***********************************************************************/
static int is_a_chain(char *name)
{
   Chain  *chain;

   return ((chain = find_chain(name)) ? 1 : 0);
}

/***********************************************************************
 *                                                                     *
 *   Returns chain name from full RZ path. The caller has to free the  *
 *   space allocated for the chain name.                               *
 *                                                                     *
 ***********************************************************************/
static char *get_chain_from_path(char *path)
{
   char  *s, *s1, *chain;
   int    l;

   if (!strncmp(path, "//", 2))
      s = path+2;
   else
      s = path;

   if ((s1 = strchr(s, '/')))
      l = s1 - s;
   else
      l = strlen(s);

   chain = calloc(l+1, 1);

   strncpy(chain, s, l);

   return chain;
}

/***********************************************************************
 *                                                                     *
 *   Return one by one the defined chains. This routine is called by   *
 *   the Kuip Browser.                                                 *
 *                                                                     *
 ***********************************************************************/
char **blist_chains(char *brobj_name, char *brcls_name, char *path, int n)
{
   static char     *obj_desc[4];
   static Chain    *chain;
   static char      name[FILE_NAME];
   static int       top;
   int              type, depth;

   obj_desc[0] = NULL;           /* name */
   obj_desc[1] = NULL;           /* class */
   obj_desc[2] = NULL;           /* object type */
   obj_desc[3] = NULL;           /* not used */

   if (!n) {
      /* initialize chain listing routine */
      if (strlen(path) == 0 || !strcmp(path, "/")) {
         chain = chain_root;
         top = 0;
      } else {
         char *s, cmd[FILE_NAME];

         top++;
         chain = NULL;
         s = strrchr(path, '/');
         s++;

         sprintf(cmd, "Cdir //%s", s);
         ku_exec(cmd);

         /*
          * must be called after the Cdir command because Cdir positions
          * the traversal at the second item
          */
         init_chain_traversal(s);
      }
   }

   while (chain) {
      /* no top chain specified, list all top level chains */
      if (toplevel_chain(chain)) {
         obj_desc[0] = chain->name;
         obj_desc[1] = "Chain";
         obj_desc[2] = "(Chain)";
         chain = chain->link;
         break;
      }
      chain = chain->link;
   }

   if (top) {
      while (next_chain_entry(name, &type, &depth)) {
         if (type == CHAIN && depth == 1) {
            obj_desc[0] = name;
            obj_desc[1] = "Chain";
            obj_desc[2] = "(Chain)";
            break;
         } else if ((type == FILE || type == LUN) && depth == 0) {
            obj_desc[0] = name;
            obj_desc[1] = "entry";
            obj_desc[2] = "(Chain Entry)";
            break;
         }
      }
   }
   return obj_desc;
}

/***********************************************************************
 *                                                                     *
 *   Fortran interface routine to add_chain_entry.                     *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
void pchadd(struct dsc$descriptor_s *dsc_tchain, int *lc,
            struct dsc$descriptor_s *dsc_tentry, int *le, int *type,
            int *istat)
{
   char *tchain = dsc_tchain->dsc$a_pointer;
   char *tentry = dsc_tentry->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
void pchadd(_fcd fcd_tchain, int *lc, _fcd fcd_tentry, int *le, int *type,
            int *istat)
{
   char *tchain = _fcdtocp(fcd_tchain);
   char *tentry = _fcdtocp(fcd_tentry);
#endif
#if defined(CERNLIB_WINNT)
void __stdcall PCHADD(char *tchain, int lentchain, int *lc, char *tentry,
                      int lententry, int *le, int *type, int *istat)
{
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_WINNT))
void pchadd(char *tchain, int *lc, char *tentry, int *le, int *type,
            int *istat)
{
#endif
   char   chain[CHAIN_NAME], entry[FILE_NAME];

   strncpy(chain, tchain, *lc); chain[*lc] = '\0';
   strncpy(entry, tentry, *le); entry[*le] = '\0';

   if (add_chain_entry(chain, entry, *type))
      *istat = 0;
   else
      *istat = 1;
}

/***********************************************************************
 *                                                                     *
 *   Fortran interface routine to delete_all_chains, delete_a_chain    *
 *   and delete_chain_entry.                                           *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
void pchdel(struct dsc$descriptor_s *dsc_tchain, int *lc,
            struct dsc$descriptor_s *dsc_tentry, int *le)
{
   char *tchain = dsc_tchain->dsc$a_pointer;
   char *tentry = dsc_tentry->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
void pchdel(_fcd fcd_tchain, int *lc, _fcd fcd_tentry, int *le)
{
   char *tchain = _fcdtocp(fcd_tchain);
   char *tentry = _fcdtocp(fcd_tentry);
#endif
#if defined(CERNLIB_WINNT)
void __stdcall PCHDEL(char *tchain, int lentchain, int*lc, char *tentry,
                      int lententry, int *le)
{
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_WINNT))
void pchdel(char *tchain, int *lc, char *tentry, int *le)
{
#endif
   char   chain[CHAIN_NAME];

   if (*lc == 0) {
      delete_all_chains();
      return;
   }

   strncpy(chain, tchain, *lc); chain[*lc] = '\0';

   if (*le == 0)
      delete_a_chain(chain);
   else {
      char  entry[FILE_NAME];
      strncpy(entry, tentry, *le); entry[*le] = '\0';
      delete_chain_entry(chain, entry);
   }
}

/***********************************************************************
 *                                                                     *
 *   Set the chain path, i.e. this path will be prefixed to all        *
 *   entries returned by pchset and pchnxt.                            *
 *   If *le=0 unset the path, *le=-1 return the currently set path in  *
 *   tentry and set *le to the length of the path, else set path.      *
 *   On error *le=-2. Do not specify a constant for *le.               *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
void pchpat(struct dsc$descriptor_s *dsc_tchain, int *lc,
            struct dsc$descriptor_s *dsc_tentry, int *le)
{
   char *tchain = dsc_tchain->dsc$a_pointer;
   char *tentry = dsc_tentry->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
void pchpat(_fcd fcd_tchain, int *lc, _fcd fcd_tentry, int *le)
{
   char *tchain = _fcdtocp(fcd_tchain);
   char *tentry = _fcdtocp(fcd_tentry);
#endif
#if defined(CERNLIB_WINNT)
void __stdcall PCHPAT(char *tchain, int lentchain, int *lc, char *tentry,
                      int lententry, int *le)
{
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_WINNT))
void pchpat(char *tchain, int *lc, char *tentry, int *le)
{
#endif
   char     name[CHAIN_NAME], entry[FILE_NAME];
   Chain   *chain;

   strncpy(name, tchain, *lc); name[*lc] = '\0';

   /* find chain */
   chain = find_chain(name);
   if (!chain) {
      printf("Chain %s does not exist.\n", name);
      *le = -2;
      return;
   }

   if (*le == 0) {
      /*
       *  delete the path from the chain
       */
      free(chain->path);
      chain->path = NULL;
   } else if (*le == -1) {
      /*
       *  return the path currently set
       */
      if (chain->path) {
         strcpy(tentry, chain->path);
         *le = strlen(chain->path);
      } else {
         *le = 0;
      }
   } else {
      strncpy(entry, tentry, *le); entry[*le] = '\0';
      chain->path = strdup(entry);
   }
}

/***********************************************************************
 *                                                                     *
 *   Fortran interface routine to print_chain.                         *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
void pchlst(struct dsc$descriptor_s *dsc_tchain, int *lc)
{
   char *tchain = dsc_tchain->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
void pchlst(_fcd fcd_tchain, int *lc)
{
   char *tchain = _fcdtocp(fcd_tchain);
#endif
#if defined(CERNLIB_WINNT)
void __stdcall PCHLST(char *tchain, int lentchain, int *lc)
{
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_WINNT))
void pchlst(char *tchain, int *lc)
{
#endif

   if (*lc == 0)
      print_chains();
   else if (tchain[*lc-1] == '>') {
      char chain[CHAIN_NAME];
      strncpy(chain, tchain, *lc-1); chain[*lc-1] = '\0';
      print_chain_tree(chain, 0);
   } else {
      char chain[CHAIN_NAME];
      strncpy(chain, tchain, *lc); chain[*lc] = '\0';
      print_chain_entries(chain);
   }
}

/***********************************************************************
 *                                                                     *
 *   Fortran interface routine to init_chain_traversal.                *
 *   It also returns the first non chain element from the chain, so    *
 *   PAW can connect (or CD in case of a LUN) to this element.         *
 *   The argument tentry should be at least FILE_NAME characters long. *
 *   In case of an error *lt is < 0: -1    chain does not exist        *
 *                                   -2    entry of type FILE or LUN   *
 *                                         does not exist              *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
void pchset(struct dsc$descriptor_s *dsc_tpath, int *lp,
            struct dsc$descriptor_s *dsc_tentry, int *lt)
{
   char *tpath  = dsc_tpath->dsc$a_pointer;
   char *tentry = dsc_tentry->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
void pchset(_fcd fcd_tpath, int *lp, _fcd fcd_tentry, int *lt)
{
   char *tpath  = _fcdtocp(fcd_tpath);
   char *tentry = _fcdtocp(fcd_tentry);
#endif
#if defined(CERNLIB_WINNT)
void __stdcall PCHSET(char *tpath, int lentpath, int *lp, char *tentry, int
lententry,
                      int *lt)
{
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_WINNT))
void pchset(char *tpath, int *lp, char *tentry, int *lt)
{
#endif
   char  entry[FILE_NAME], path[FILE_NAME];
   char *chain;
   int   type, depth;

   /* in case chain does not exist *lt will be -1 */
   *lt = -1;

   if (*lp == 0)
      return;

   strncpy(path, tpath, *lp); path[*lp] = '\0';

   chain = get_chain_from_path(path);

   init_chain_traversal(chain);

   if (!current_chain) {
      free(chain);
      return;
   }

   /* find first non CHAIN entry */
   while (next_chain_entry(entry, &type, &depth)) {
      if (type == FILE || type == LUN) {
         *lt = strlen(entry);
         if (current_path) {
            int l = strlen(current_path) - 1;
            if (current_path[l] == '/' || current_path[l] == '\\' ||
                current_path[l] == ']' || current_path[l] == ':'  ||
                current_path[l] == '>')
               sprintf(tentry, "%s", current_path);
            else
               sprintf(tentry, "%s/", current_path);
            strncat(tentry, entry, *lt);
            *lt = strlen(tentry);
         } else
            strncpy(tentry, entry, *lt);
         free(chain);
         return;
      }
   }

   /* in case no entry of type FILE or LUN exists *lt will be -2 */
   *lt = -2;

   free(chain);
}

/***********************************************************************
 *                                                                     *
 *   Return next chain entry of type FILE or LUN. When no next element *
 *   is found *lt = 0. Also return the name of the current chain.      *
 *   Make sure that entry and chain strings are at least FILE_NAME and *
 *   CHAIN_NAME long. No error checking is done.                       *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
void pchnxt(struct dsc$descriptor_s *dsc_chain, int *lc,
            struct dsc$descriptor_s *dsc_tentry, int *lt)
{
   char *chain  = dsc_chain->dsc$a_pointer;
   char *tentry = dsc_tentry->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
void pchnxt(_fcd fcd_chain, int *lc, _fcd fcd_tentry, int *lt)
{
   char *chain  = _fcdtocp(fcd_chain);
   char *tentry = _fcdtocp(fcd_tentry);
#endif
#if defined(CERNLIB_WINNT)
void __stdcall PCHNXT(char *chain, int lenchain, int *lc, char *tentry,
                      int lententry, int *lt)
{
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_WINNT))
void pchnxt(char *chain, int *lc, char *tentry, int *lt)
{
#endif
   char  entry[FILE_NAME];
   int   type, depth;

   /* return first non CHAIN entry */
   while (next_chain_entry(entry, &type, &depth)) {
      if (type == FILE || type == LUN) {
         *lt = strlen(entry);
         if (current_path) {
            int l = strlen(current_path) - 1;
            if (current_path[l] == '/' || current_path[l] == '\\' ||
                current_path[l] == ']' || current_path[l] == ':')
               sprintf(tentry, "%s", current_path);
            else
               sprintf(tentry, "%s/", current_path);
            strncat(tentry, entry, *lt);
            *lt = strlen(tentry);
         } else
            strncpy(tentry, entry, *lt);
         *lc = strlen(current_chain->name);
         strncpy(chain, current_chain->name, *lc);
         return;
      }
   }

   *lt = 0;
}

/***********************************************************************
 *                                                                     *
 *   Returns 1 if NAME is a chain else 0.                              *
 *   NAME can be of the form //CHAIN/DIR or just CHAIN.                *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
int pischn(struct dsc$descriptor_s *dsc_tname, int *ln)
{
   char *tname  = dsc_tname->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
int pischn(_fcd fcd_tname, int *ln)
{
   char *tname  = _fcdtocp(fcd_tname);
#endif
#if defined(CERNLIB_WINNT)
int __stdcall PISCHN(char *tname, int lentname, int *ln)
{
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_WINNT))
int pischn(char *tname, int *ln)
{
#endif
   char  name[FILE_NAME], *chain;
   int   l;

   strncpy(name, tname, *ln); name[*ln] = '\0';

   chain = get_chain_from_path(name);

   l = is_a_chain(chain);

   free(chain);

   return l;
}

/***********************************************************************
 *                                                                     *
 *   Returns 1 if NAME is a piaf chain, 0 when not piaf and -1 when,   *
 *   illegal, mixed mode.                                              *
 *   A piaf chain is a chain of which all elements refer to a file on  *
 *   the piaf system. Either all elements have the form //piaf/... or  *
 *   the current_path starts with //piaf.                              *
 *   NAME can be of the form //CHAIN/DIR or just CHAIN.                *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
int piscpf(struct dsc$descriptor_s *dsc_tname, int *ln)
{
   char *tname  = dsc_tname->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
int piscpf(_fcd fcd_tname, int *ln)
{
   char *tname  = _fcdtocp(fcd_tname);
#endif
#if defined(CERNLIB_WINNT)
int __stdcall PISCPF(char *tname, int lentname, int *ln)
{
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_WINNT))
int piscpf(char *tname, int *ln)
{
#endif
   char  entry[FILE_NAME], name[FILE_NAME];
   int   type, depth, i = 0, n = 0;
   char *chain;

   strncpy(name, tname, *ln); name[*ln] = '\0';

   chain = get_chain_from_path(name);

   init_chain_traversal(chain);

   if (!current_chain) {
      free(chain);
      return 0;
   }

   /* check if a non chain entry starts with //piaf */
   while (next_chain_entry(entry, &type, &depth)) {
      if (type == FILE || type == LUN) {
         i++;
         if ((current_path && !strncasecmp(current_path, "//piaf", 6)) ||
             !strncasecmp(entry, "//piaf/", 7))
            n++;
      }
   }

   free(chain);

   if (i && i == n) return  1;
   if (i && n == 0) return  0;
   if (i && i != n) return -1;
   return 0;
}

/***********************************************************************
 *                                                                     *
 *   Sets the number of events in a chain for Ntuple id to nevents or  *
 *   adds nevents to the current number of events, depending on op.    *
 *   Op can be SET (=1) or ADD (=2). The function returns the current  *
 *   number of events (after the operation has been performed).        *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
int pchevt(struct dsc$descriptor_s *dsc_tname, int *ln, int *id,
           int *nevents, int *op)
{
   char *tname  = dsc_tname->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
int pchevt(_fcd fcd_tname, int *ln, int *id,
           int *nevents, int *op)
{
   char *tname  = _fcdtocp(fcd_tname);
#endif
#if defined(CERNLIB_WINNT)
int __stdcall PCHEVT(char *tname, int lentname, int *ln, int *id,
                     int *nevents, int *op)
{
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_WINNT))
int pchevt(char *tname, int *ln, int *id, int *nevents, int *op)
{
#endif
    char   *chain_name, name[FILE_NAME];
    Chain  *chain;
    int     i, evt = -1;

    strncpy(name, tname, *ln); name[*ln] = '\0';

    chain_name = get_chain_from_path(name);

    chain = find_chain(chain_name);
    if (chain) {
       i = find_ntuple(chain, *id, name);
       if (i < 0) {
          if (chain->no_ntuples == 0)
             chain->ntuples = (Ntuple *)malloc(sizeof(Ntuple));
          else
             chain->ntuples = (Ntuple *)realloc(chain->ntuples,
                               (chain->no_ntuples+1)*sizeof(Ntuple));
          chain->no_ntuples++;
          i = chain->no_ntuples - 1;
          chain->ntuples[i].id   = *id;
          chain->ntuples[i].dir  = strdup(name);
          chain->ntuples[i].rows = 0;
       }
       switch (*op) {
          case SET:
             chain->ntuples[i].rows = *nevents;
             break;
          case ADD:
             chain->ntuples[i].rows += *nevents;
             break;
       }
       evt = chain->ntuples[i].rows;
    }

    free(chain_name);

    return evt;
}

/***********************************************************************
 *                                                                     *
 *   Returns the number of files or luns in the chains tree.           *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
int pchcnt(struct dsc$descriptor_s *dsc_tname, int *ln)
{
   char *tname  = dsc_tname->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
int pchcnt(_fcd fcd_tname, int *ln)
{
   char *tname  = _fcdtocp(fcd_tname);
#endif
#if defined(CERNLIB_WINNT)
int __stdcall PCHCNT(char *tname, int lentname, int *ln)
{
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_WINNT))
int pchcnt(char *tname, int *ln)
{
#endif
   char  entry[FILE_NAME], name[FILE_NAME];
   char *chain;
   int   type, depth, n = 0;

   if (*ln == 0)
      return n;

   strncpy(name, tname, *ln); name[*ln] = '\0';

   chain = get_chain_from_path(name);

   init_chain_traversal(chain);

   if (!current_chain) {
      free(chain);
      return n;
   }

   /* count the number of non CHAIN entries */
   while (next_chain_entry(entry, &type, &depth)) {
      if (type == FILE || type == LUN)
         n++;
   }

   free(chain);

   return n;
}

/***********************************************************************
 *                                                                     *
 *   Return the first top chain.                                       *
 *   This routine is used by the Piaf master (where at most one chain  *
 *   at a time is defined).                                            *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_VAX)
void pchtop(struct dsc$descriptor_s *dsc_tchain, int *lc)
{
   char *tchain = dsc_tchain->dsc$a_pointer;
#endif
#if defined(CERNLIB_CRAY)
void pchtop(_fcd fcd_tchain, int *lc)
{
   char *tchain = _fcdtocp(fcd_tchain);
#endif
#if defined(CERNLIB_WINNT)
void __stdcall PCHTOP(char *tchain, int lentchain, int *lc)
{
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_WINNT))
void pchtop(char *tchain, int *lc)
{
#endif

   if (chain_root) {
      *lc = strlen(chain_root->name);
      strncpy(tchain, chain_root->name, *lc);
   } else
      *lc = -1;
}
