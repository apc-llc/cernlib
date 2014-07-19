/*
 * $Id: khash.h,v 1.1.1.1 1996/03/08 15:33:00 mclareni Exp $
 *
 * $Log: khash.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:00  mclareni
 * Kuip
 *
 */
/* khash.h: hash table management */

typedef struct _HashArray {
  char       *name;             /* symbol name */
  void       *value;            /* symbol value */
  int         tag;
} HashArray;

typedef struct _HashEntry {
  struct _HashEntry *next;      /* link to next entry */
  char       *name;             /* symbol name */
  void       *value;            /* symbol value */
  int         tag;
} HashEntry;

typedef struct {
  int         size;             /* table size should be a prime number */
  HashEntry **entries;          /* pointer to array of size entries */
  int        nentries;          /* number of entries */
  int         is_string;        /* flag if strdup/free(value) should be used */
  int         tag_only;         /* flag if only tag field is used */
  int         case_convert;
  int       (*strcmp)();        /* strcmp/strcasecmp */
  int       (*hash_cmp)();      /* hash_cmp/hash_casecmp */
} HashTable;

extern HashArray* hash_array(   HashTable* );
extern void       hash_clear(   HashTable* );
extern void       hash_config(  HashTable*, const char* );
extern HashTable* hash_create(  int );
extern void       hash_destroy( HashTable* );
extern int        hash_entries( HashTable* );
extern void       hash_insert(  HashTable*, const char*, const void*, int );
extern void*      hash_lookup(  HashTable*, const char*, int* );
extern void*      hash_remove(  HashTable*, const char* );


